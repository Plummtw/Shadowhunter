package org.plummtw.shadowhunter.heavy

import scala.xml._
import net.liftweb._
import net.liftweb.mapper._
import http._
import js._
import util._
import common._
import S._
import SHtml._
import Helpers._


import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.actor._
import org.plummtw.shadowhunter.snippet._
import org.plummtw.shadowhunter.data._
import org.plummtw.shadowhunter.util.{PlummUtil, LocationHelper}

object ActionHelper extends Logger {
  def action_list(roomphase: RoomPhase, currentuserentry : UserEntry) : List[ActionData] = {
    //roomphase = RoomPhase_R.get
    if (roomphase.phase_type.is == RoomPhaseEnum.GAMEHALL.toString)
      List(ActionKick, ActionStartGame) // ActionTestAlert
    else if (roomphase.phase_type.is == RoomPhaseEnum.ENDED.toString)
      List()
    else {
      // currentuserentry = CurrentUserEntry_R.get
      val role = currentuserentry.get_role
      if (roomphase.player.is != currentuserentry.id.is)
        role.free_skill :: List(ActionFlip, ActionItemPreferred)
      else if (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString)
        role.free_skill :: role.movement_skill ::: List(ActionFlip, ActionMove, ActionCassandraGive, ActionItemPreferred)
      else if (roomphase.phase_type.is == RoomPhaseEnum.LOCATION.toString) {
        if (roomphase.phase_flags != "")
          List(ActionCardChoose)
        else role.free_skill :: List(ActionFlip, ActionDrawBlackCard, ActionDrawWhiteCard, ActionDrawGreenCard, 
                                     ActionLocDamage, ActionLocHeal, ActionLocRob, ActionNoLoc, 
                                     ActionItemPreferred)
      } else if ((roomphase.phase_type.is == RoomPhaseEnum.CARD.toString) || 
               (roomphase.phase_type.is == RoomPhaseEnum.CARD_SKILL.toString)) {
        val card_type = CardEnum.get_card(roomphase.phase_flags.is).cardtype_enum
        val card_skill = card_type match {
          case CardTypeEnum.BLACK => List(ActionBlackCard)
          case CardTypeEnum.WHITE => List(ActionWhiteCard)
          case CardTypeEnum.GREEN => List(ActionGreenCard)
        }
        role.free_skill :: card_skill
      } else if (roomphase.phase_type.is == RoomPhaseEnum.POST_ATTACK.toString) {
        role.post_skill :: List(ActionNextRound)
      } else
        role.attack_skill :: List(ActionFlip, ActionAttack, ActionNoAttack, ActionItemPreferred)
    }  
  }
  
  def enabled_action_list(room:Room, roomround:RoomRound, roomphase:RoomPhase, 
                          currentuserentry:UserEntry, userentrys_rr:List[UserEntry]) : List[ActionData] = {
    action_list(roomphase, currentuserentry).filter {x => 
      if (!x.enabled(room, roomround, roomphase, currentuserentry, userentrys_rr)) false
      else {
        if (x.isInstanceOf[UserEntryTargetable])
          (x.asInstanceOf[UserEntryTargetable].targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr).length != 0)
        else if  (x.isInstanceOf[LocationTargetable])
          (x.asInstanceOf[LocationTargetable].targetable_locations(room, roomround, roomphase, currentuserentry, userentrys_rr).length != 0)
        else if  (x.isInstanceOf[CardTargetable])
          (x.asInstanceOf[CardTargetable].targetable_cards(room, roomround, roomphase, currentuserentry, userentrys_rr).length != 0)
        else
          true
      }
    }
  }
  
  def process_action(action: Action, room: Room, roomround: RoomRound,
                   roomphase:RoomPhase, actioner: UserEntry, userentrys: List[UserEntry]) = {
    val actioner_id = action.actioner_id.is
    val action_enum = MTypeEnum.get_action(action.mtype.is)

    action_enum match {
      case MTypeEnum.ACTION_KICK =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        actionee.inflict_damage(1, actioner)
        actionee.save

        // 如果被踢到 3 次
        val room = Room.find(By(Room.id, actionee.room_id.is)).get

        if (actionee.damaged.is >= 3) {
          UserEntrySnippet.revoke(room, actionee)
          RoomActor.sendUserEntryMessage(actionee.id.is, ForceOut(actionee.id.is))
        }
        val userentrys_reload = UserEntry.findAllByRoom(room)
        RoomActor.sendRoomMessage(actionee.room_id.is, SessionVarSet(room = room, userentrys = userentrys_reload))
        RoomActor.sendRoomMessage(actionee.room_id.is, RoomForceUpdate(actionee.room_id.is ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.ACTION_BAR)))

      case MTypeEnum.ACTION_STARTGAME =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        actioner.add_room_flag(UserEntryRoomFlagEnum.VOTED)
        actioner.save

        // 如果全員都開始遊戲
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        val userentrys_r = UserEntry.rr(userentrys)
        val userentrys_notready = userentrys_r.filter(x => (x.hasnt_room_flag(UserEntryRoomFlagEnum.VOTED)))

        if ((userentrys_r.length >= 4) && (userentrys_notready.length == 0)) {
          val room = Room.find(By(Room.id, actioner.room_id.is)).get
          room.status(RoomStatusEnum.PLAYING.toString)
          room.save
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room))
          
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is,
                                                                         List(ForceUpdateEnum.GO_OUT_LINK)))

          GameProcessor.process_start_game(room)
          // New Round
          //val room_reload       = Room.find(By(Room.id, actioner.room_id.is)).get
          val roomround_reload  = RoomRound.find(By(RoomRound.room_id, room.id.is),
                                                 OrderBy(RoomRound.round_no, Descending)).get
          val roomphase_reload  = RoomPhase.find(By(RoomPhase.roomround_id, roomround_reload.id.is),
                                                 OrderBy(RoomPhase.phase_no, Descending)).get
          val userentrys_reload = UserEntry.findAllByRoom(room)

          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomround = roomround_reload,
                                                                        roomphase = roomphase_reload,
                                                                        userentrys = userentrys_reload))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is,
                                                                          List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.LOCATION_TABLE,
                                                                               ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.ACTION_BAR)))
        } else {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }
      case MTypeEnum.ACTION_FLIP =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val role = actioner.get_role
        
        if (actioner.has_user_flag(UserEntryFlagEnum.ADVENT) && (role.role_side == RoleSideEnum.HUNTER))
          actioner.damaged(0)
        if (actioner.has_user_flag(UserEntryFlagEnum.DIABOLIC) && (role.role_side == RoleSideEnum.SHADOW))
          actioner.damaged(0)
        if (actioner.has_user_flag(UserEntryFlagEnum.CHOCOLATE)) {
          val life_thresh = role.role_side match {
            case RoleSideEnum.NEUTRAL => 8
            case RoleSideEnum.SHADOW => 11
            case RoleSideEnum.HUNTER => 11
          }
            
          if (role.role_life <= life_thresh) 
            actioner.damaged(0)
        }
        
        if (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) {
          if ((role == RoleCatherine) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)))
            actioner.lower_damage(1)
          if ((role == RoleGinger) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED))) {
            val shadow_rl = userentrys.filter(x => (x.live.is) && (!x.revoked.is) && (x.get_role.role_side == RoleSideEnum.SHADOW)) 
            val hunter_rl = userentrys.filter(x => (x.live.is) && (!x.revoked.is) && (x.get_role.role_side == RoleSideEnum.HUNTER)) 
            if (shadow_rl.length > hunter_rl.length)
              actioner.lower_damage(2)
            }
        }
        
        //val role = actioner.get_role
        if ((role.role_side == RoleSideEnum.SHADOW) && 
           (userentrys.filter(x => (x.get_role.role_side == RoleSideEnum.SHADOW) && (x.revealed.is)).length == 0)) {
          val live_evans = userentrys.filter(x => (x.live.is) && (x.get_role == RoleEvan))
          if (live_evans.length != 0) {
            val live_evan = live_evans(0)
            live_evan.add_user_flag(UserEntryFlagEnum.LOVER)
            live_evan.target_user(actioner.id.is)
            actioner.add_user_flag(UserEntryFlagEnum.LOVER)
            
            if (room.has_flag(RoomFlagEnum.EVAN_HEAL)) {
              if (actioner.live.is) 
                actioner.damaged(0)
              live_evan.damaged(0)
            }
          
            
            if (!live_evan.revealed.is)
              GameProcessor.flip(live_evan, action, userentrys)
            else
              live_evan.save
          }
        }
        
        actioner.revealed(true)
        actioner.save
        
        val live_unrevealed = userentrys.filter(x => (x.get_role != RoleDetective) &&(x.live.is) && (!x.revealed.is) && (!x.revoked.is))
        if (live_unrevealed.length == 0) {
          val live_detectives = userentrys.filter(x =>(x.get_role == RoleDetective) && (x.live.is))
          live_detectives.foreach { live_detective =>
            val saved_damaged = live_detective.damaged.is
            live_detective.damaged(99)
            GameProcessor.check_death(live_detective, live_detective, action, userentrys)
            live_detective.damaged(saved_damaged).save
          }
        }

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }
      case MTypeEnum.ACTION_MOVE =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get

        var random1d6 = GameProcessor.random.nextInt(6) + 1
        var random1d4 = GameProcessor.random.nextInt(4) + 1
        var new_location = LocationEnum.from_dice(random1d6 + random1d4)
        
        while (new_location.toString == actioner.location.is.toString) {
          random1d6 = GameProcessor.random.nextInt(6) + 1
          random1d4 = GameProcessor.random.nextInt(4) + 1
          new_location = LocationEnum.from_dice(random1d6 + random1d4)
        }
        
        val (new_location_str : String, display_location_str : String) =
          if ((actioner.get_role == RoleEmi) && (actioner.revealed) &&
              //(LocationHelper.neighbor(room, actioner.location.is.toString) == action.action_flags.is.toString) &&
              ((LocationHelper.left(room, actioner.location.is.toString) == action.action_flags.is.toString) ||
               (LocationHelper.right(room, actioner.location.is.toString) == action.action_flags.is.toString)) &&
              (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)))
            (action.action_flags.is.toString, LocationEnum.TELEPORT.toString)
          else  {
            if ((actioner.get_role == RoleEmi) && (actioner.revealed) &&
                (room.has_flag(RoomFlagEnum.EMI_ENHANCE)))
              actioner.add_role_flag(UserEntryRoleFlagEnum.ENHANCED)
          
            if (new_location.toString == LocationEnum.OPTION.toString)
              (action.action_flags.is.toString, LocationEnum.OPTION.toString)
            else if (actioner.has_item(CardEnum.W_MYSTIC_COMPASS) && (math.abs(random1d6 - random1d4) == 1))
              (action.action_flags.is.toString, LocationEnum.COMPASS.toString)
            else if (new_location.toString == actioner.location.is.toString)
              (action.action_flags.is.toString, LocationEnum.REPEAT.toString)
            else
              (new_location.toString, new_location.toString)
          }

        
        actioner.location(new_location_str)
        
        val vengeful_ghosts = userentrys.filter(x =>
          ((x.location.is == new_location_str) || 
           ((room.has_flag(RoomFlagEnum.VGHOST_EXPAND)) && (x.location.is == LocationHelper.neighbor(room, new_location_str))))
          &&
          (x.live.is) && (x.revealed.is) && (x.get_role == RoleVengefulGhost) &&
          (x.hasnt_user_flag(UserEntryFlagEnum.SEALED)))
        
        var update_enum = List(ForceUpdateEnum.LOCATION_TABLE, ForceUpdateEnum.TIME_TABLE)
         
        val vengeful_ghost_str =
          if (((!actioner.revealed) || (actioner.get_role.role_side != RoleSideEnum.SHADOW)) && (vengeful_ghosts.length != 0)) {
            if (actioner.inflict_damage(2, vengeful_ghosts(0))) {
              GameProcessor.check_death(actioner, vengeful_ghosts(0), action, userentrys)
              update_enum = List(ForceUpdateEnum.LOCATION_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)
              "，並受到 2 點損傷"
            } else ""
          } else ""
        
        val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_MOVE.toString)
                       .message("1D6=" + random1d6 + ",1D4=" + random1d4 + " 移動結果：" + LocationEnum.get_cname(display_location_str) + vengeful_ghost_str)
        talk.save
        talk.send(actioner.room_id.is)
        
        actioner.save

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          if (!actioner.live.is)
            GameProcessor.next_player(room, roomround, roomphase, userentrys)
          else {
            val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                     .phase_type(RoomPhaseEnum.LOCATION.toString).player(roomphase.player.is)
                                     .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
            new_phase.save
            RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys = userentrys))
            RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, update_enum))
            RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
          }
        }
      case MTypeEnum.ACTION_ATTACK =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //var roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get

        val death_number = userentrys.filter(x => !x.live.is).length
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        val (attack_power, attack_str) = GameProcessor.attack(actioner, actionee, action, userentrys)
        
        val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_ATTACK.toString)
                              .message(attack_str)
        talk.save
        talk.send(actioner.room_id.is)

        if ((actionee.get_role == RoleADecoy) && (actionee.revealed.is)) {
          if (actioner.inflict_damage(attack_power, actionee, true))
            GameProcessor.check_death(actioner, actionee, action, userentrys)
          if (!actioner.live.is)
            actionee.add_user_flag(UserEntryFlagEnum.VICTORY).save
        }
        else if (actionee.inflict_damage(attack_power, actioner, true))
          GameProcessor.check_death(actionee, actioner, action, userentrys)
        
        val is_ambush =         
          if ((!actionee.revealed.is) && (actionee.get_role == RoleWerewolf) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
              (actionee.has_role_flag(UserEntryRoleFlagEnum.AMBUSH))){
            GameProcessor.flip(actionee, action, userentrys)
            true  
          } else false
        
        val is_adecoy =
          ((room.has_flag(RoomFlagEnum.ADECOY_INTIMATE)) &&
           (actioner.get_role == RoleADecoy) && (actioner.revealed.is) &&
           (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (attack_power == 0))
        
        val is_actionee_live = (actionee.live.is)
          
        // 狼人反擊
        if (is_adecoy || ((actionee.revealed.is) && (actionee.get_role == RoleWerewolf) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)))) {
          val action1 = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_WEREWOLF_COUNTER.toString)
                              .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
          action1.save
          
          val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_WEREWOLF_COUNTER.toString)
                              .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
                              .message_flags(if(is_ambush)MTypeEnum.ACTION_WEREWOLF_AMBUSH.toString else "")
          talk1.save
          talk1.send(actioner.room_id.is)
          
          val (attack_power_1, attack_str_1) = GameProcessor.attack(actionee, actioner, action, userentrys, is_ambush)
          
          val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_ATTACK.toString)
                                .message(attack_str_1)
          talk2.save
          talk2.send(actioner.room_id.is)

          if ((actioner.get_role == RoleADecoy) && (actioner.revealed.is)) {
            if (actionee.inflict_damage(attack_power_1, actioner, true))
              GameProcessor.check_death(actionee, actioner, action, userentrys)
            if ((!actionee.live.is) && (is_actionee_live))
              actioner.add_user_flag(UserEntryFlagEnum.VICTORY).save
          } else if (actioner.inflict_damage(attack_power_1, actionee, true))
            GameProcessor.check_death(actioner, actionee, action, userentrys)
        }

        // 進行下一玩家
        //val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
        //                               OrderBy(RoomPhase.phase_no, Descending)).get
        //val userentrys   = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        //val userentrys_rl = userentrys.filter(x => (!x.revoked.is) && (x.live.is))
        val death_number2 = userentrys.filter(x => !x.live.is).length
        if ((actioner.get_role == RoleBomb) && (death_number2 - death_number >= 2)) {
          userentrys.filter(_.get_role == RoleBomb).foreach { userentry1 =>
            userentry1.add_user_flag(UserEntryFlagEnum.VICTORY).save
          }
        }  

        // 檢查遊戲是否結束
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          post_attack_or_next_player(room, roomround, roomphase, userentrys)
          /*
          var player_index = userentrys_rl.map(_.id.is).indexOf(roomphase.player.is)
          var new_phase_no = roomphase.phase_no.is + 1
          var new_roomround = roomround

          if ((player_index + 1) >= userentrys_rl.length) {
            // 新回合
            player_index = 0
            new_phase_no = 1

            new_roomround = RoomRound.create.room_id(room.id.is).round_no(roomround.round_no.is + 1)
                                     .last_round(roomround.id.is)
            new_roomround.save

            val talk = Talk.create.roomround_id(new_roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                           .message("第 " + (new_roomround.round_no.is.toString) + " 日 "+ (new java.util.Date).toString)
            talk.save
          } else
            player_index = player_index + 1
          
          val next_player = userentrys_rl(player_index)
          next_player.remove_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)
          if ((next_player.revealed) && (next_player.get_role == RoleCatherine) && (next_player.hasnt_user_flag(UserEntryFlagEnum.SEALED)))
            next_player.damaged(math.max(0, next_player.damaged.is - 1))
          next_player.save

          val new_phase = RoomPhase.create.roomround_id(new_roomround.id.is).phase_no(new_phase_no).additional(roomphase.additional.is)
                                   .phase_type(RoomPhaseEnum.MOVEMENT.toString).player(next_player.id.is)
                                   .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.move_time.is))
          new_phase.save
          
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomround = new_roomround, roomphase = new_phase, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR)))
          */
        }
      
      case MTypeEnum.ACTION_FENG_KIKOU =>

        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        val kikou_damage =
          if (actionee.items.length <= 1)
            2
          else 
            1
        
        if (actionee.inflict_damage(kikou_damage, actioner))
          GameProcessor.check_death(actionee, actioner, action, userentrys)
        
        // 檢查遊戲是否結束
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          post_attack_or_next_player(room, roomround, roomphase, userentrys)
        }  
        
      case MTypeEnum.ACTION_MULTIATTACK =>
        val actionee_s = ActionMultiAttack.targetable_users(room, roomround, roomphase, actioner, userentrys)
        val death_number = userentrys.filter(x => !x.live.is).length
        
        actionee_s.foreach { actionee => 
          val (attack_power, attack_str) = GameProcessor.attack(actioner, actionee, action, userentrys)
        
          val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_ATTACK.toString)
                               .message("對 " + actionee.handle_name.is +  "：" +attack_str)
          talk.save
          talk.send(actioner.room_id.is)

          if ((actionee.get_role == RoleADecoy) && (actionee.revealed.is)) {
            if (actioner.inflict_damage(attack_power, actionee, true))
              GameProcessor.check_death(actioner, actionee, action, userentrys)
            if (!actioner.live.is)
              actionee.add_user_flag(UserEntryFlagEnum.VICTORY).save
          } else if (actionee.inflict_damage(attack_power, actioner, true))
            GameProcessor.check_death(actionee, actioner, action, userentrys)
        
          val is_ambush =         
          if ((!actionee.revealed.is) && (actionee.get_role == RoleWerewolf) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
              (actionee.has_role_flag(UserEntryRoleFlagEnum.AMBUSH))){
            GameProcessor.flip(actionee, action, userentrys)
            true  
          } else false

          val is_adecoy =
            ((room.has_flag(RoomFlagEnum.ADECOY_INTIMATE)) &&
             (actioner.get_role == RoleADecoy) && (actioner.revealed.is) &&
             (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (attack_power == 0))

          val is_actionee_live = (actionee.live.is)
          
          // 狼人反擊
          if (is_adecoy || ((actionee.revealed.is) && (actionee.get_role == RoleWerewolf) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)))) {
            val action1 = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_WEREWOLF_COUNTER.toString)
                               .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
            action1.save
          
            val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_WEREWOLF_COUNTER.toString)
                                  .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
                                  .message_flags(if(is_ambush)MTypeEnum.ACTION_WEREWOLF_AMBUSH.toString else "")
            talk1.save
            talk1.send(actioner.room_id.is)
          
            val (attack_power_1, attack_str_1) = GameProcessor.attack(actionee, actioner, action, userentrys, is_ambush)
          
            val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_ATTACK.toString)
                                  .message(attack_str_1)
            talk2.save
            talk2.send(actioner.room_id.is)

            if ((actioner.get_role == RoleADecoy) && (actioner.revealed.is)) {
              if (actionee.inflict_damage(attack_power_1, actioner, true))
                GameProcessor.check_death(actionee, actioner, action, userentrys)
              if ((!actionee.live.is) && is_actionee_live)
                actioner.add_user_flag(UserEntryFlagEnum.VICTORY).save
            } else if (actioner.inflict_damage(attack_power_1, actionee, true))
              GameProcessor.check_death(actioner, actionee, action, userentrys)
          }
        }
        val death_number2 = userentrys.filter(x => !x.live.is).length
        if ((actioner.get_role == RoleBomb) && (death_number2 - death_number >= 2)) {
          userentrys.filter(_.get_role == RoleBomb).foreach { userentry1 =>
            userentry1.add_user_flag(UserEntryFlagEnum.VICTORY).save
          }
        }  
        
        // 檢查遊戲是否結束
        if (!GameProcessor.check_victory(room, roomround, userentrys))
          post_attack_or_next_player(room, roomround, roomphase, userentrys)
        
      case MTypeEnum.ACTION_NOATTACK =>
        // 進行下一玩家
        post_attack_or_next_player(room, roomround, roomphase, userentrys)
        
      case MTypeEnum.ACTION_DRAWBLACKCARD =>
        CardHelper.process_drawblack(action, room, roomround, roomphase, actioner, userentrys)
        
      case MTypeEnum.ACTION_DRAWWHITECARD => 
        CardHelper.process_drawwhite(action, room, roomround, roomphase, actioner, userentrys)
        
      case MTypeEnum.ACTION_DRAWGREENCARD => 
        val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                .phase_type(RoomPhaseEnum.CARD.toString).player(roomphase.player.is).phase_flags(action.action_flags.is)
                                .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
        new_phase.save
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.TIME_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        
      case MTypeEnum.ACTION_LOCDAMAGE =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        if (actionee.hasnt_item(CardEnum.W_FORTUNE_BROOCH)) {
          val locdamage_damage = 
            if ((actionee.get_role == RoleUnseen) && (actionee.revealed.is) &&
               (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)))1
            else 2
          
          if (actionee.inflict_damage(locdamage_damage, actioner)) {
            if ((actioner.get_role == RoleWitch) && (actioner.revealed.is) &&
                (!actioner.has_user_flag(UserEntryFlagEnum.SEALED))) 
              actionee.add_user_flag(UserEntryFlagEnum.FROG)
            GameProcessor.check_death(actionee, actioner, action, userentrys)
          }
        }
        
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                   .phase_type(RoomPhaseEnum.ATTACK.toString).player(roomphase.player.is)
                                   .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
          new_phase.save
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }  

      case MTypeEnum.ACTION_LOCHEAL =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        actionee.lower_damage(1)
        actionee.save
        
        val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                 .phase_type(RoomPhaseEnum.ATTACK.toString).player(roomphase.player.is)
                                 .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
        new_phase.save
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys = userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        
      case MTypeEnum.ACTION_LOCROB =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        if ((action.action_flags.is != "") &&
            (actionee.has_item(CardEnum.get_card(action.action_flags.is).card_enum))) {
          val robbed_item = CardEnum.get_card(action.action_flags.is)
          GameProcessor.rob_specific(actioner, actionee, robbed_item)  
        } else
          GameProcessor.rob_single(actioner, actionee)
        actionee.save
        
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                   .phase_type(RoomPhaseEnum.ATTACK.toString).player(roomphase.player.is)
                                   .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
          new_phase.save
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }          

      case MTypeEnum.ACTION_NOLOC =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is).additional(roomphase.additional.is)
                                 .phase_type(RoomPhaseEnum.ATTACK.toString).player(roomphase.player.is)
                                 .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
        new_phase.save
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.TIME_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))  

      case MTypeEnum.ACTION_WHITECARD =>
        CardHelper.process_white(action, room, roomround,
                                 roomphase, actioner, userentrys)
 
      case MTypeEnum.ACTION_BLACKCARD =>
        CardHelper.process_black(action, room, roomround,
                                 roomphase, actioner, userentrys)        
        
      case MTypeEnum.ACTION_GREENCARD =>
        CardHelper.process_green(action, room, roomround,
                                 roomphase, actioner, userentrys)
        
      case MTypeEnum.ACTION_ALLIE_MOTHERLOVE =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        actioner.damaged(4)
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
      case MTypeEnum.ACTION_WEREWOLF_AMBUSH =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        actioner.add_role_flag(UserEntryRoleFlagEnum.AMBUSH)
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))  
      
      case MTypeEnum.ACTION_ULTRASOUL_RAY =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        if (actionee.inflict_damage(1, actioner))
          GameProcessor.check_death(actionee, actioner, action, userentrys)

        // 檢查死亡和勝利條件
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        //val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get
        //val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
        //                               OrderBy(RoomPhase.phase_no, Descending)).get

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }  
        
      case MTypeEnum.ACTION_ULTRASOUL_URAY =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        if (actionee.inflict_damage(3, actioner))
          GameProcessor.check_death(actionee, actioner, action, userentrys)

        // 檢查死亡和勝利條件
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        //val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get
        //val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
        //                               OrderBy(RoomPhase.phase_no, Descending)).get

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }
        
      case MTypeEnum.ACTION_ELLEN_CURSECHAIN =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        actionee.add_user_flag(UserEntryFlagEnum.SEALED)
        actionee.save
        
        if (room.has_flag(RoomFlagEnum.ELLEN_HEAL) && (!actionee.revealed.is))
          actioner.lower_damage(4)
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        
      case MTypeEnum.ACTION_FRANKLIN_LIGHTNING =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        val damage = try {
          action.action_flags.is.toInt}
          catch {case e : Exception => 0}

        if (actionee.inflict_damage(damage, actioner))
          GameProcessor.check_death(actionee, actioner, action, userentrys)

        // 檢查死亡和勝利條件
        if (room.has_flag(RoomFlagEnum.FRANKLIN_REUSE) &&(damage <= 2))
          actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)
        else   
          actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)
        actioner.save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        //val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get
        //val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
        //                               OrderBy(RoomPhase.phase_no, Descending)).get

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }

      case MTypeEnum.ACTION_FUKA_DYNAMITEHEAL =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        val actionee_role = actionee.get_role

        val damaged =
          if ((actionee.revealed.is) && (actionee_role.role_side == RoleSideEnum.HUNTER))
            6
          else if ((actionee.revealed.is) && (actionee_role.role_side == RoleSideEnum.SHADOW))
            8
          else
            7
            
        actionee.damaged(damaged)
        actionee.save
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        
      case MTypeEnum.ACTION_GEORGE_DEMOLISH =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        val damage = try {
          action.action_flags.is.toInt}
          catch {case e : Exception => 0}

        if (actionee.inflict_damage(damage, actioner))
          GameProcessor.check_death(actionee, actioner, action, userentrys)

        // 檢查死亡和勝利條件
        if (room.has_flag(RoomFlagEnum.GEORGE_REUSE) && (damage <= 1))
          actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)
        else
          actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)
        actioner.save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        //val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get
        //val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
        //                               OrderBy(RoomPhase.phase_no, Descending)).get

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }
        
      case MTypeEnum.ACTION_GREGOR_BARRIER =>
        actioner.add_user_flag(UserEntryFlagEnum.BARRIER)
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save
        
        GameProcessor.next_player(room, roomround, roomphase, userentrys)
        
      case MTypeEnum.ACTION_WIGHT_MANIPULATE =>
        val additional = try {
          action.action_flags.is.toInt
        } catch { case e: Exception => 0 }
        
        roomphase.additional(roomphase.additional.is + additional).save
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save
        
        GameProcessor.next_player(room, roomround, roomphase, userentrys)  
      
      case MTypeEnum.ACTION_NEXTROUND =>
        GameProcessor.next_player(room, roomround, roomphase, userentrys)  
        
      case MTypeEnum.ACTION_CHARLES_BLOODFEAST =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        val (attack_power, attack_str) = GameProcessor.attack(actioner, actionee, action, userentrys)
        
        val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_ATTACK.toString)
                              .message(attack_str)
        talk.save
        talk.send(actioner.room_id.is)

        if ((actionee.get_role == RoleADecoy) && (actionee.revealed.is)) {
          if (actioner.inflict_damage(attack_power, actionee, true))
            GameProcessor.check_death(actioner, actionee, action, userentrys)
          if (!actioner.live.is)
            actionee.add_user_flag(UserEntryFlagEnum.VICTORY).save
        } else if (actionee.inflict_damage(attack_power, actioner, true))
          GameProcessor.check_death(actionee, actioner, action, userentrys)
        if (actioner.inflict_damage(2, actioner))
          GameProcessor.check_death(actioner, actioner, action, userentrys)

        val is_ambush =         
          if ((!actionee.revealed.is) && (actionee.get_role == RoleWerewolf) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
              (actionee.has_role_flag(UserEntryRoleFlagEnum.AMBUSH))){
            GameProcessor.flip(actionee, action, userentrys)
            true  
          } else false
        
        // 狼人反擊
        if ((actionee.revealed.is) && (actionee.get_role == RoleWerewolf) && (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED))) {
          val action1 = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_WEREWOLF_COUNTER.toString)
                              .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
          action1.save
          
          val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_WEREWOLF_COUNTER.toString)
                              .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
                              .message_flags(if(is_ambush)MTypeEnum.ACTION_WEREWOLF_AMBUSH.toString else "")
          talk1.save
          talk1.send(actioner.room_id.is)
          
          val (attack_power_1, attack_str_1) = GameProcessor.attack(actionee, actioner, action, userentrys, is_ambush)
          
          val talk2 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_ATTACK.toString)
                                .message(attack_str_1)
          talk2.save
          talk2.send(actioner.room_id.is)

          if (actioner.inflict_damage(attack_power_1, actionee, true))
            GameProcessor.check_death(actioner, actionee, action, userentrys)
        }
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        // 檢查遊戲是否結束
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          post_attack_or_next_player(room, roomround, roomphase, userentrys)
        }
        
      case MTypeEnum.ACTION_UNKNOWN_DECEIVE =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        //val role = RoleEnum.get_role(action.action_flags.is.toString)
        
        actioner.role_flags(action.action_flags.is)
        actioner.save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
        //RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
        //RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))  
      case MTypeEnum.ACTION_DAVID_GRAVEDIG =>
        
        val card = CardPool.find(By(CardPool.room_id, room.id.is),
                                 By(CardPool.card, action.action_flags.is)).get
        card.owner_id(actioner.id.is).discarded(false).save
        
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)
        actioner.add_item(CardEnum.get_card(action.action_flags.is).card_enum)
        actioner.save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        
        if (GameProcessor.check_item_victory(actioner)) 
          GameProcessor.check_victory(room, roomround, userentrys)
        else {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }
      case MTypeEnum.ACTION_FATHEROCONNEL_PRAY =>
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)
        actioner.save
        CardHelper.process_drawwhite(action, room, roomround, roomphase, actioner, userentrys)
        
        
      case MTypeEnum.ACTION_CASSANDRA_FATECHANGE =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)
        actioner.target_user(actionee_id)
        
        val item = CardEnum.get_card(action.action_flags.is).card_enum
        
        actionee.add_item(item)
        actioner.remove_item(item)
        
        val card = CardPool.find(By(CardPool.room_id, actionee.room_id.is),
                                 By(CardPool.card, item.toString)).get
        card.owner_id(actionee_id).discarded(false).save
        
        actioner.save
        actionee.save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        
        if (GameProcessor.check_item_victory(actionee))
          GameProcessor.check_victory(room, roomround, userentrys)
        else {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.ACTION_BAR)))
          RoomActor.sendUserEntryMessage(actionee.room_id.is, RoomForceUpdate(actionee.room_id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }
        
      case MTypeEnum.ACTION_CASSANDRA_GIVE =>
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        
        actionee.target_user(actioner_id)
        
        val item = CardEnum.get_card(action.action_flags.is).card_enum
        
        actionee.add_item(item)
        actioner.remove_item(item)
        
        val card = CardPool.find(By(CardPool.room_id, actioner.room_id.is),
                                 By(CardPool.card, item.toString)).get
        card.owner_id(actioner_id).discarded(false).save
        
        actioner.save
        actionee.save
        
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        
        if (GameProcessor.check_item_victory(actionee))
          GameProcessor.check_victory(room, roomround, userentrys)
        else {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.ACTION_BAR)))
          RoomActor.sendUserEntryMessage(actionee.room_id.is, RoomForceUpdate(actionee.room_id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }
      
      case MTypeEnum.ACTION_ITEMPREFERRED =>
        actioner.item_preferred(action.action_flags.is).save
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
        
      case MTypeEnum.ACTION_BOMB_BOMB =>
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED).save
        val death_number = userentrys.filter(x => !x.live.is).length
        userentrys.foreach { userentry1 =>
          if (userentry1.location.is == action.action_flags.is.toString)
            if (userentry1.inflict_damage(1, actioner))
              GameProcessor.check_death(userentry1, actioner, action, userentrys)
        }
        val death_number2 = userentrys.filter(x => !x.live.is).length
        if (death_number2 - death_number >= 2)
          actioner.add_user_flag(UserEntryFlagEnum.VICTORY).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        
        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          if (!actioner.live.is)
            GameProcessor.next_player(room, roomround, roomphase, userentrys) 
          else {
            RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
            RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.USER_TABLE)))
            RoomActor.sendUserEntryMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.ACTION_BAR)))
          }  
        }
        
      case MTypeEnum.ACTION_ANGEL_REINCARNATE =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        //val actionee : UserEntry = userentrys.filter(_.id.is == actionee_id)(0)

        // 檢查死亡和勝利條件
        actioner.target_user(actionee_id).save

        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
        
      case MTypeEnum.ACTION_EVAN_BRACEUP =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        val damage = try {
          action.action_flags.is.toInt}
          catch {case e : Exception => 0}

        actionee.remove_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save

        // 檢查死亡和勝利條件
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        //val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get
        //val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
        //                               OrderBy(RoomPhase.phase_no, Descending)).get

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          //RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }  
        
      case MTypeEnum.ACTION_ADECOY_TAUNT =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED).save
        actionee.add_user_flag(UserEntryFlagEnum.TAUNT).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        //val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get
        //val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
        //                               OrderBy(RoomPhase.phase_no, Descending)).get

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }  
      
      case MTypeEnum.ACTION_GODFAT_EXCHANGE =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
        val actionee2 : UserEntry = userentrys.filter(_.id.is.toString == action.action_flags.is)(0)

        val saved_user_no = actionee.user_no.is
        actionee.user_no(actionee2.user_no.is).save
        actionee2.user_no(saved_user_no).save
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED).save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        //val room = Room.find(By(Room.id, actioner.room_id.is)).get
        //val userentrys = UserEntry.findAll(By(UserEntry.room_id, actioner.room_id.is))
        //val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
        //                               OrderBy(RoomRound.round_no, Descending)).get
        //val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
        //                               OrderBy(RoomPhase.phase_no, Descending)).get

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = UserEntry.findAllByRoom(room)))
          RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        } 
        
      case MTypeEnum.ACTION_DETECTIVE_REASONA =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        actionee.add_user_flag(UserEntryFlagEnum.REASONAED).save
        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)
        
        
        if (actionee.get_real_role.role_side.toString == action.action_flags.is) {
          actioner.action_point(actioner.action_point.is + 2)
          if (actioner.action_point.is >= userentrys.filter(!_.revoked.is).length)
            actioner.add_user_flag(UserEntryFlagEnum.VICTORY)
                    .add_user_flag(UserEntryFlagEnum.VICTORY2)
        }
        
        actioner.save
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          //RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }   

      case MTypeEnum.ACTION_DETECTIVE_REASONR =>
        //val actioner  = UserEntry.find(By(UserEntry.id, actioner_id)).get
        val actionee_id = action.actionee_id.is
        val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)

        actioner.add_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)
        
        if (actionee.get_real_role.role_enum.toString == action.action_flags.is) {
          actioner.add_user_flag(UserEntryFlagEnum.VICTORY)
                  .add_user_flag(UserEntryFlagEnum.VICTORY2) 
        }
        roomphase.deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.reaction_time.is)).save
        
        actioner.save

        if (!GameProcessor.check_victory(room, roomround, userentrys)) {
          RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, userentrys = userentrys))
          //RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is ,List(ForceUpdateEnum.USER_TABLE)))
          RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
        }   
        
      case xs =>
        warn("Unprocessed Action : " + action.toString)
        
        
    }
  }
  
  def post_attack_or_next_player(room : Room, roomround : RoomRound, roomphase : RoomPhase, userentrys : List[UserEntry]) = {
    val actioner = UserEntry.get(roomphase.player.is, userentrys)
    val role = actioner.get_role
    
    if (!actioner.live.is)
      GameProcessor.next_player(room, roomround, roomphase, userentrys)
    else if ((actioner.revealed) && (actioner.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)) &&
        (role.post_skill != ActionNoAction)) {
      val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                 .phase_type(RoomPhaseEnum.POST_ATTACK.toString).player(roomphase.player.is)
                                 .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
      new_phase.save
      RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys = userentrys))
      RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TIME_TABLE)))
      RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
    } else
      GameProcessor.next_player(room, roomround, roomphase, userentrys)
  }
}
