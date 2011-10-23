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
import org.plummtw.shadowhunter.card._
import org.plummtw.shadowhunter.util.{PlummUtil, LocationHelper}

object CardHelper extends Logger {
  def card_table(room : Room, cardpools : List[CardPool]) = {
    if (cardpools.length == 0)
      <span></span>
    else {
      val black_remains = cardpools.filter(x => (x.card_no.is >= room.blackcard_index.is)
                                           && (x.discarded.is == false) && (x.card_type.is == CardTypeEnum.BLACK.toString))
      val white_remains = cardpools.filter(x => (x.card_no.is >= room.whitecard_index.is)
                                           && (x.discarded.is == false) && (x.card_type.is == CardTypeEnum.WHITE.toString))
      val green_remains = cardpools.filter(x => (x.card_no.is >= room.greencard_index.is)
                                           && (x.discarded.is == false) && (x.card_type.is == CardTypeEnum.GREEN.toString))
      
      val black_discards = cardpools.filter(x => (x.discarded.is == true) && (x.card_type.is == CardTypeEnum.BLACK.toString))
      val white_discards = cardpools.filter(x => (x.discarded.is == true) && (x.card_type.is == CardTypeEnum.WHITE.toString))
      val green_discards = cardpools.filter(x => (x.discarded.is == true) && (x.card_type.is == CardTypeEnum.GREEN.toString))
      
      <span>
        牌庫剩餘：黑：{black_remains.length.toString} 白：{white_remains.length.toString} 綠：{green_remains.length.toString}
        棄牌區：黑：{black_discards.length.toString} 白：{white_discards.length.toString} 綠：{green_discards.length.toString}    
      </span>
    }
  }
  
  def process_green_internal(actioner : UserEntry, actionee : UserEntry, card : Card) : Talk = {
    //val actioner_role = actioner.get_role
    val actionee_role = actionee.get_hermit_role
    
    if (card.card_enum == CardEnum.G_REVEAL_PREV) {
      val actionee_role2 =
        if (actioner.get_role == RoleDetective)
          actionee.get_real_role
        if (actionee_role == RoleNoEffect) RoleUnknown
        else actionee_role
        
       val talk1 = Talk.create.mtype(MTypeEnum.RESULT_GREENREVEAL.toString)
                        .message("你發現 " + actionee.handle_name.is + " 是 " + actionee_role.role_name)
                        .actioner_id(actioner.id.is).actionee_id(actionee.id.is)
       talk1                 
    } else {
      val message =
        card.card_enum match {
          case CardEnum.G_SHADOW_LOSE1        => 
            if (actionee_role.role_side == RoleSideEnum.SHADOW) {
              if (actionee.inflict_card_damage(1, actioner))
                actionee.handle_name.is + " 受到 1 點損傷"
              else ""
            } else ""
          case CardEnum.G_SHADOW_LOSE2        => 
            if (actionee_role.role_side == RoleSideEnum.SHADOW) {
              if (actionee.inflict_card_damage(2, actioner))
                actionee.handle_name.is + " 受到 2 點損傷"
              else ""
            } else ""
          case CardEnum.G_HUNTER_LOSE1         => 
            if (actionee_role.role_side == RoleSideEnum.HUNTER) {
              if (actionee.inflict_card_damage(1, actioner))
                actionee.handle_name.is + " 受到 1 點損傷"
              else ""
            } else ""
          case CardEnum.G_SHADOW_HUNTER_EQUIP  => 
            if ((actionee_role.role_side == RoleSideEnum.SHADOW) ||
                (actionee_role.role_side == RoleSideEnum.HUNTER)) {
              if (actionee.item_preferred.is == CardEnum.PREFER_LIFE.toString) {
                if (actionee.inflict_card_damage(1, actioner))
                  actionee.handle_name.is + " 受到 1 點損傷"
                else ""
              } else if ((actionee.item_preferred.is != "") &&
                         (actionee.has_item(CardEnum.get_card(actionee.item_preferred.is).card_enum))) {
                val robbed_item = CardEnum.get_card(actionee.item_preferred.is)
                GameProcessor.rob_specific(actioner, actionee, robbed_item)
                actionee.save
                actionee.handle_name.is + " 被搶奪 " + robbed_item.card_name
              } else if (actionee.items.length > 0) {
                val robbed_item = GameProcessor.rob_single(actioner, actionee)
                actionee.save
                actionee.handle_name.is + " 被搶奪 " + robbed_item.card_name
              } else {
                if (actionee.inflict_card_damage(1, actioner))
                  actionee.handle_name.is + " 受到 1 點損傷"
                else ""
              }
            } else ""
          case CardEnum.G_SHADOW_NEUTRAL_EQUIP => 
            if ((actionee_role.role_side == RoleSideEnum.SHADOW) ||
                (actionee_role.role_side == RoleSideEnum.NEUTRAL)) {
              if (actionee.item_preferred.is == CardEnum.PREFER_LIFE.toString) {
                if (actionee.inflict_card_damage(1, actioner))
                  actionee.handle_name.is + " 受到 1 點損傷"
                else ""
              } else if ((actionee.item_preferred.is != "") &&
                         (actionee.has_item(CardEnum.get_card(actionee.item_preferred.is).card_enum))) {
                val robbed_item = CardEnum.get_card(actionee.item_preferred.is)
                GameProcessor.rob_specific(actioner, actionee, robbed_item)
                actionee.save
                actionee.handle_name.is + " 被搶奪 " + robbed_item.card_name
              } else if (actionee.items.length > 0) {
                val robbed_item = GameProcessor.rob_single(actioner, actionee)
                actionee.save
                actionee.handle_name.is + " 被搶奪 " + robbed_item.card_name
              } else {
                if (actionee.inflict_card_damage(1, actioner))
                  actionee.handle_name.is + " 受到 1 點損傷"
                else ""
              }
            } else ""
          case CardEnum.G_HUNTER_NEUTRAL_EQUIP => 
            if ((actionee_role.role_side == RoleSideEnum.HUNTER) ||
                (actionee_role.role_side == RoleSideEnum.NEUTRAL)) {
              if (actionee.item_preferred.is == CardEnum.PREFER_LIFE.toString) {
                if (actionee.inflict_card_damage(1, actioner))
                  actionee.handle_name.is + " 受到 1 點損傷"
                else ""
              } else if ((actionee.item_preferred.is != "") &&
                         (actionee.has_item(CardEnum.get_card(actionee.item_preferred.is).card_enum))) {
                val robbed_item = CardEnum.get_card(actionee.item_preferred.is)
                GameProcessor.rob_specific(actioner, actionee, robbed_item)
                actionee.save
                actionee.handle_name.is + " 被搶奪 " + robbed_item.card_name
              } else if (actionee.items.length > 0) {
                val robbed_item = GameProcessor.rob_single(actioner, actionee)
                actionee.save
                actionee.handle_name.is + " 被搶奪 " + robbed_item.card_name
              } else {
                if (actionee.inflict_card_damage(1, actioner))
                  actionee.handle_name.is + " 受到 1 點損傷"
                else ""
              }
            } else ""
          case CardEnum.G_SHADOW_HEAL1         => 
            if ((actionee_role.role_side == RoleSideEnum.SHADOW) ||
                ((actionee_role.role_side == RoleSideEnum.NONE) && (actionee.damaged.is != 0))) {
              if (actionee.damaged.is == 0) {
                if (actionee.inflict_card_damage(1, actioner))
                  actionee.handle_name.is + " 受到 1 點損傷"
                else ""
              } else {
                actionee.lower_damage(1)
                actionee.handle_name.is + " 回復 1 點損傷"
              } 
            } else ""
          case CardEnum.G_HUNTER_HEAL2         => 
            if ((actionee_role.role_side == RoleSideEnum.HUNTER) ||
                ((actionee_role.role_side == RoleSideEnum.NONE) && (actionee.damaged.is != 0))) {
              if (actionee.damaged.is == 0) {
                if (actionee.inflict_card_damage(2, actioner))
                  actionee.handle_name.is + " 受到 2 點損傷"
                else ""
              } else {
                actionee.lower_damage(2)
                actionee.handle_name.is + " 回復 2 點損傷"
              } 
            } else ""
          case CardEnum.G_HUNTER_HEAL1         => 
            if ((actionee_role.role_side == RoleSideEnum.HUNTER) ||
                ((actionee_role.role_side == RoleSideEnum.NONE) && (actionee.damaged.is != 0))) {
              if (actionee.damaged.is == 0) {
                if (actionee.inflict_card_damage(1, actioner))
                  actionee.handle_name.is + " 受到 1 點損傷"
                else ""
              } else {
                actionee.lower_damage(1)
                actionee.handle_name.is + " 回復 1 點損傷"
              } 
            } else ""  
          case CardEnum.G_NEUTRAL_HEAL1        => 
            if ((actionee_role.role_side == RoleSideEnum.NEUTRAL) ||
                ((actionee_role.role_side == RoleSideEnum.NONE) && (actionee.damaged.is != 0))) {
              if (actionee.damaged.is == 0) {
                if (actionee.inflict_card_damage(1, actioner))
                  actionee.handle_name.is + " 受到 1 點損傷"
                else ""
              } else {
                actionee.lower_damage(1)
                actionee.handle_name.is + " 回復 1 點損傷"
              } 
            } else ""
          case CardEnum.G_LIFE_UNDER11_2         => 
            if ((actionee_role.role_life <= 11) ||
                ((actionee_role.role_side == RoleSideEnum.NONE) && (actionee.damaged.is != 0))) {
              if (actionee.damaged.is == 0) {
                if (actionee.inflict_card_damage(2, actioner))
                  actionee.handle_name.is + " 受到 2 點損傷"
                else ""
              } else {
                actionee.lower_damage(2)
                actionee.handle_name.is + " 回復 2 點損傷"
              } 
            } else ""  
          case CardEnum.G_LIFE_OVER12          => 
            if ((actionee_role.role_life >= 12) && (actionee_role != RoleNoEffect))  {
              if (actionee.inflict_card_damage(2, actioner))
                actionee.handle_name.is + " 受到 2 點損傷"
              else ""
            } else ""
          case CardEnum.G_LIFE_UNDER11         => 
            if ((actionee_role.role_life <= 11) && (actionee_role != RoleNoEffect))  {
              if (actionee.inflict_card_damage(1, actioner))
                actionee.handle_name.is + " 受到 1 點損傷"
              else ""
            } else ""
        }
      val talk1_message = if (message == "") "但是什麼也沒發生" else message
      val talk1 = Talk.create.mtype(MTypeEnum.RESULT_GREENCARD.toString)
                      .message(talk1_message)
                      .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
      talk1
    }
  }
  
  def process_green(action: Action, room: Room, roomround: RoomRound,
                     roomphase:RoomPhase, actioner: UserEntry, userentrys: List[UserEntry]) = {
    val actionee_id = action.actionee_id.is
    val actionee_list : List[UserEntry] = userentrys.filter(_.id.is == actionee_id)
    
    if (actionee_list.length == 0) {
      warn("process_green : actionee_list.length == 0")
      warn("room : " + room.id.is)
      warn("roomround : " + roomround.id.is)
      warn("roomphase : " + roomphase.id.is)
      warn("actioner : " + actioner.id.is)
      warn("actionee : " + actionee_id)
    }
    
    val actionee = actionee_list(0)
    val actionee_role = actionee.get_role
        
    val card = CardEnum.get_card(roomphase.phase_flags.is.toString)
    
    val talk1 = process_green_internal(actioner, actionee, card)    
    talk1.roomround_id(roomround.id.is)
    talk1.save
    talk1.send(actioner.room_id.is)
    
    //GameProcessor.check_item_victory(actioner)

    GameProcessor.check_death(actionee, actioner, action, userentrys)
    // 檢查遊戲是否結束
    if (!GameProcessor.check_victory(room, roomround, userentrys)) {

      val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                               .phase_type(RoomPhaseEnum.ATTACK.toString).player(roomphase.player.is)
                               .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
      new_phase.save
      RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys = userentrys))
      RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TIME_TABLE)))
      RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))  
    }
  }
  
  def process_drawwhite(action: Action, room: Room, roomround: RoomRound,
                          roomphase:RoomPhase, actioner: UserEntry, userentrys: List[UserEntry]) = {
    if ((actioner.get_role == RoleEmma) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED))) {
      val live_hunters = userentrys.filter (x => (x.get_role.role_side == RoleSideEnum.HUNTER) &&
                                                 (x.live.is) && (x.revealed.is))
      live_hunters.foreach { _.lower_damage(1).save }
    }
    
    val card = CardEnum.get_card(action.action_flags.is)
    
    val phase_type =
      if (card.isInstanceOf[Equipment]) {
        actioner.add_item(card.card_enum)
        actioner.save
        GameProcessor.check_item_victory(actioner)
        RoomPhaseEnum.ATTACK
      } else if (card.isInstanceOf[UserEntryTargetable]) {
        RoomPhaseEnum.CARD
      } else {
        card.card_enum match {
          case CardEnum.W_HOLY_WATER_OF_HEALING =>
            actioner.lower_damage(2)
            actioner.save
          case CardEnum.W_ADVENT                =>
            val role = actioner.get_role
            if (role.role_side == RoleSideEnum.HUNTER) {
              if (actioner.revealed) 
                actioner.damaged(0) 
              else
                actioner.add_user_flag(UserEntryFlagEnum.ADVENT)
              actioner.save 
            }
          case CardEnum.W_CHOCOLATE             =>
            val role = actioner.get_role
            val life_thresh = role.role_side match {
              case RoleSideEnum.NEUTRAL => 8
              case RoleSideEnum.SHADOW => 11
              case RoleSideEnum.HUNTER => 11
            }
            
            if (role.role_life <= life_thresh) {
              if (actioner.revealed) 
                actioner.damaged(0) 
              else
                actioner.add_user_flag(UserEntryFlagEnum.CHOCOLATE)
              actioner.save 
            }
          case CardEnum.W_CONCEALED_KNOWLEDGE   =>
            roomphase.additional(roomphase.additional.is + 1).save
          case CardEnum.W_GUARDIAN_ANGEL        =>
            actioner.add_user_flag(UserEntryFlagEnum.GUARDIAN)
            actioner.save
          case CardEnum.W_FLARE_OF_JUDGEMENT    =>
            val userentrys_rrs = userentrys.filter(x => (!x.revoked.is) && (x.live.is) && (x.id.is != actioner.id.is))
            val death_number = userentrys.filter(x => !x.live.is).length
            userentrys_rrs.foreach { userentry1 =>
              if (userentry1.inflict_card_damage(2, actioner))
                GameProcessor.check_death(userentry1, actioner, action, userentrys)
            }
            val death_number2 = userentrys.filter(x => !x.live.is).length
            if ((actioner.get_role == RoleBomb) && (death_number2 - death_number >= 2)) {
              userentrys.filter(_.get_role == RoleBomb).foreach { userentry1 =>
                userentry1.add_user_flag(UserEntryFlagEnum.VICTORY)
              }
            }  
          case CardEnum.W_DISENCHANTED_MIRROR   =>
            val role = actioner.get_role
            if ((!actioner.revealed) && (role.role_side == RoleSideEnum.SHADOW) &&
                (role.role_life >= 12)) {
              GameProcessor.flip(actioner, action, userentrys)
            }
          case CardEnum.W_TEA   =>
            val items_num = actioner.items.length
            if (items_num == 0)
              actioner.lower_damage(3).save
            else {
              if (actioner.inflict_card_damage(items_num, actioner))
                GameProcessor.check_death(actioner, actioner, action, userentrys)
            }
              
            
        }
        RoomPhaseEnum.ATTACK
      }
    
    val phase_type2 = 
      if (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) {
        if (phase_type == RoomPhaseEnum.CARD)
          RoomPhaseEnum.CARD_SKILL
        else if (phase_type == RoomPhaseEnum.ATTACK)
          RoomPhaseEnum.MOVEMENT
        else phase_type
      } else phase_type
    
    if (!GameProcessor.check_victory(room, roomround, userentrys)) {
      val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                  .phase_type(phase_type2.toString).player(roomphase.player.is).phase_flags(action.action_flags.is)
                                  .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
      new_phase.save
      RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys=userentrys))
      RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)))
      RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
    }  
  }
                       
  def process_white(action: Action, room: Room, roomround: RoomRound,
                    roomphase:RoomPhase, actioner: UserEntry, userentrys: List[UserEntry]) = {
    val actionee_id = action.actionee_id.is
    val actionee : UserEntry = userentrys.filter(_.id.is == actionee_id)(0)
    
    val card = CardEnum.get_card(action.action_flags.is)
    card.card_enum match {
      case CardEnum.W_BLESSING =>
        val heal1d6 = GameProcessor.random.nextInt(6) + 1

        actionee.lower_damage(heal1d6)
        actionee.save
        
        val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_WHITECARD.toString)
                        .message("1D6=" + heal1d6 + "," + actionee.handle_name.is + " 回復 " + heal1d6 + " 點損傷")
                        .actioner_id(actionee.id.is).actionee_id(actioner.id.is)
        talk1.save
        talk1.send(actioner.room_id.is)

      case CardEnum.W_FIRST_AID =>

        actionee.damaged(7)
        actionee.save
    }
    
    val phase_type = 
      if (roomphase.phase_type.is == RoomPhaseEnum.CARD_SKILL.toString) 
        RoomPhaseEnum.MOVEMENT
      else
        RoomPhaseEnum.ATTACK

    if (!GameProcessor.check_victory(room, roomround, userentrys)) {
      if (!actioner.live.is)
        GameProcessor.next_player(room, roomround, roomphase, userentrys)
      else {
        val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                 .phase_type(phase_type.toString).player(roomphase.player.is).phase_flags(action.action_flags.is)
                                 .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
        new_phase.save
    
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys=userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
      }
    }
  }
  
  def process_drawblack(action: Action, room: Room, roomround: RoomRound,
                          roomphase:RoomPhase, actioner: UserEntry, userentrys: List[UserEntry]) = {
    if ((actioner.get_role == RoleEmma) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED))) {
      val live_shadows = userentrys.filter (x => (x.get_role.role_side == RoleSideEnum.SHADOW) &&
                                                 (x.live.is) && (x.revealed.is))
      live_shadows.foreach { live_shadow =>
        live_shadow.inflict_damage(1, actioner)
        GameProcessor.check_death(live_shadow, actioner, action, userentrys)
      }
    }
    
    if (actioner.get_role != RoleCheshire) {
      val cheshires = userentrys.filter(_.get_role == RoleCheshire)
      cheshires.foreach { cheshire =>
        if (cheshire.target_user.is == 0)
          cheshire.target_user(actioner.id.is).save
      }
    }
    
    val card = CardEnum.get_card(action.action_flags.is)
    
    val phase_type =
      if (card.isInstanceOf[Equipment]) {
        actioner.add_item(card.card_enum)
        actioner.save
        GameProcessor.check_item_victory(actioner)
        RoomPhaseEnum.ATTACK
      } else if (card.isInstanceOf[UserEntryTargetable]) {
        RoomPhaseEnum.CARD
      } else {
        card.card_enum match {
          case CardEnum.B_DYNAMITE =>
            val userentrys_r = userentrys.filter(x => (!x.revoked.is) && (x.live.is))
            val death_number = userentrys.filter(x => !x.live.is).length
            val random1d6 = GameProcessor.random.nextInt(6) + 1
            val random1d4 = GameProcessor.random.nextInt(4) + 1
            
            val new_location = LocationEnum.from_dice(random1d6 + random1d4)
            val location_str = 
              if (new_location == LocationEnum.OPTION)
                "失敗"
              else
                "目標地點：" + LocationEnum.get_cname(new_location)
            
            val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                              .message("1D6=" + random1d6 + ",1D4=" + random1d4 + " " + location_str)
            talk1.save
            talk1.send(actioner.room_id.is)
              
            userentrys_r.foreach { userentry1 =>
              if (userentry1.hasnt_item(CardEnum.W_TALISMAN) && (userentry1.location.is == new_location.toString))
                if (userentry1.inflict_card_damage(3, actioner))
                  GameProcessor.check_death(userentry1, actioner, action, userentrys)
            }
            
            val death_number2 = userentrys.filter(x => !x.live.is).length
            if ((actioner.get_role == RoleBomb) && (death_number2 - death_number >= 2)) {
              userentrys.filter(_.get_role == RoleBomb).foreach { userentry1 =>
                userentry1.add_user_flag(UserEntryFlagEnum.VICTORY)
              }
            }
          case CardEnum.B_DIABOLIC_RITUAL                =>
            val role = actioner.get_role
            if (role.role_side == RoleSideEnum.SHADOW) {
              if (actioner.revealed) 
                actioner.damaged(0) 
              else
                actioner.add_user_flag(UserEntryFlagEnum.DIABOLIC)
              actioner.save 
            }
          case CardEnum.B_BANANA_PEEL             =>
            if (actioner.items.length > 0) {
              val userentrys_r = userentrys.filter(x => (!x.revoked.is) && (x.live.is))
              val index = userentrys_r.indexOf(actioner)
              val prev_index = (index + userentrys_r.length - 1) % (userentrys_r.length)
              val prev_userentry = userentrys_r(prev_index)

              val robbed_item = GameProcessor.rob_single(prev_userentry, actioner)
              val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                      .message(actioner.handle_name.is + " 給 " + prev_userentry.handle_name.is + " " + CardEnum.get_card(robbed_item.card_enum).card_name)
              talk1.save
              talk1.send(actioner.room_id.is)
              
              //GameProcessor.check_item_victory(prev_userentry) 
            } else {
              if (actioner.inflict_card_damage(1, actioner))
                GameProcessor.check_death(actioner, actioner, action, userentrys)
    
              val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                              .message(actioner.handle_name.is + " 受到 1 點損傷")
              talk1.save
              talk1.send(actioner.room_id.is)
            }
            actioner.save
        }
        RoomPhaseEnum.ATTACK
      }
    
    if (!GameProcessor.check_victory(room, roomround, userentrys)) {
      if (!actioner.live.is)
        GameProcessor.next_player(room, roomround, roomphase, userentrys)
      else {
        val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                    .phase_type(phase_type.toString).player(roomphase.player.is).phase_flags(action.action_flags.is)
                                    .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
        new_phase.save
      
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys=userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
      }
    }
  }  
  
  def process_black(action: Action, room: Room, roomround: RoomRound,
                    roomphase:RoomPhase, actioner: UserEntry, userentrys: List[UserEntry]) = {
    val actionee_id = action.actionee_id.is
    val actionee : UserEntry = UserEntry.get(actionee_id, userentrys)
    
    val card = CardEnum.get_card(action.action_flags.is)
    card.card_enum match {
      case CardEnum.B_VAMPIRE_BAT            =>
        if (actionee.hasnt_item(CardEnum.W_TALISMAN)) {
          if (actionee.inflict_card_damage(2, actioner))
            GameProcessor.check_death(actionee, actioner, action, userentrys)
          actioner.lower_damage(1)
          actioner.save
        }
      case CardEnum.B_BLOODTHIRSTY_SPIDER    =>
        val death_number = userentrys.filter(x => !x.live.is).length
        if (actionee.hasnt_item(CardEnum.W_TALISMAN)) {
          if (actionee.inflict_card_damage(2, actioner))
            GameProcessor.check_death(actionee, actioner, action, userentrys)
        }
        if (actioner.hasnt_item(CardEnum.W_TALISMAN)) {
          if (actioner.inflict_card_damage(2, actioner))
            GameProcessor.check_death(actioner, actioner, action, userentrys)
        }
        val death_number2 = userentrys.filter(x => !x.live.is).length
        if ((actioner.get_role == RoleBomb) && (death_number2 - death_number >= 2)) {
          userentrys.filter(_.get_role == RoleBomb).foreach { userentry1 =>
            userentry1.add_user_flag(UserEntryFlagEnum.VICTORY)
          }
        }  
      case CardEnum.B_MOODY_GOBLIN          =>
        val message = 
          if (actionee.items.length > 0) {
            val robbed_item = GameProcessor.rob_single(actioner, actionee)
            actionee.save
            "搶奪道具： " + CardEnum.get_card(robbed_item.card_enum).card_name
          } else "但是什麼也沒有搶到"
                                        
        val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                    .message(message)
        talk1.save
        talk1.send(actioner.room_id.is)
      case CardEnum.B_SPIRITUAL_DOLL          =>
        val random1d6 = GameProcessor.random.nextInt(6) + 1
        val target = if (random1d6 < 5) actionee else actioner
        
        val talk1 = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.RESULT_BLACKCARD.toString)
                              .message("1D6=" + random1d6 + ", " + target.handle_name.is + " 受到 3 點損傷")
        talk1.save
        talk1.send(actioner.room_id.is)
        
        if (target.inflict_card_damage(3, actioner))
          GameProcessor.check_death(target, actioner, action, userentrys)
      case CardEnum.B_LAMIRROR          =>
        if (!actionee.revealed.is)
          GameProcessor.flip(actionee, action, userentrys)
    }

    if (!GameProcessor.check_victory(room, roomround, userentrys)) {
      if (!actioner.live.is)
        GameProcessor.next_player(room, roomround, roomphase, userentrys)
      else {
        val new_phase = RoomPhase.create.roomround_id(roomround.id.is).phase_no(roomphase.phase_no.is + 1).additional(roomphase.additional.is)
                                    .phase_type(RoomPhaseEnum.ATTACK.toString).player(roomphase.player.is).phase_flags(action.action_flags.is)
                                    .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.action_time.is))
        new_phase.save
      
        RoomActor.sendRoomMessage(actioner.room_id.is, SessionVarSet(room = room, roomphase = new_phase, userentrys=userentrys))
        RoomActor.sendRoomMessage(actioner.room_id.is, RoomForceUpdate(actioner.room_id.is, List(ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.USER_TABLE)))
        RoomActor.sendUserEntryMessage(actioner.id.is, UserEntryForceUpdate(actioner.id.is, List(ForceUpdateEnum.ACTION_BAR)))
      }
    }
  }
}
