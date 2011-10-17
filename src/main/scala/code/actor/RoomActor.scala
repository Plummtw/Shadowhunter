package org.plummtw.shadowhunter.actor

import _root_.net.liftweb._
import net.liftweb.mapper._
import http._
import SHtml._
import js._
import JsCmds._
import common._
import util._
import Helpers._
import actor._

//import scala.xml.NodeSeq

import collection.mutable.HashMap

import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.util._
import org.plummtw.shadowhunter.comet._
import org.plummtw.shadowhunter.data._
import org.plummtw.shadowhunter.heavy._

case class RoomSubscribe(comet : GameComet, room_id : Long, userentry_id : Long)
case class RoomUnsubscribe(comet : GameComet, room_id : Long, userentry_id : Long)
case class RoomDeleteRoom(room_id : Long)

case class NewMessage(room_id : Long, talk : Talk)
case class SignalAction(action : Action)
case class SignalAbandon(room : Room)
case class SignalDeadline(room : Room, roomround : RoomRound, roomphase : RoomPhase)

case class RoomForceOut(room_id : Long)
case class ForceOut(userentry_id : Long)
case class ForceLogOut(userentry_id : Long)

case class SessionVarSet(room : Room = null, roomround : RoomRound = null, roomphase : RoomPhase = null, 
                         userentrys : List[UserEntry] = List(), card_list : List[CardPool] = List())
case class RoomForceUpdate(room_id : Long, updates : List[ForceUpdateEnum.Value])
case class UserEntryForceUpdate(userentry_id : Long, updates : List[ForceUpdateEnum.Value])

//case class NewRoomRound(room_id : Long, roomround : RoomRound)

object RoomActor extends LiftActor with Logger {
  private val userentry_message_map : HashMap[Long, GameComet] = new HashMap()
  private val reverse_map : HashMap[GameComet, (Long, Long)] = new HashMap()
  
  def sendUserEntryMessage(id : Long, message : Any) = {
    val comet_list = userentry_message_map.get(id) match {
      case Some(some_userentry) => some_userentry ! message
      case _                    => ; //warn("No UserEntry Found")
    }
  }
    
  private val room_message_map : HashMap[Long, List[GameComet]] = new HashMap()
  def sendRoomMessage(id : Long, message : Any) = {
    val comet_list = room_message_map.get(id) match {
      case Some(message_list) => message_list.foreach(_ ! message)
      case _                  => ; //warn("No Room Found")
    }
    
    message match {
      case SessionVarSet(room, roomround, roomphase, userentrys, card_list) =>
        ClockActor ! message
      case x => ;
    }
  }
  
  override def messageHandler = (in:Any) => in match {
    case RoomSubscribe(comet, room_id, userentry_id) =>
      //println("RoomSubscribe : " + comet.toString + " " + room_id + " " + userentry_id)
      var comet_list = room_message_map.get(room_id) match {
        case Some(message_list) => message_list
        case _                  => List()
      }
      if (userentry_message_map.contains(userentry_id)) {
        val old_comet = userentry_message_map.get(userentry_id).get
        if (old_comet != comet) {
          old_comet ! ForceLogOut(userentry_id)
          comet_list = comet_list filterNot (_ == old_comet)
        }
      }
      if (reverse_map.contains(comet)) {
        val (old_room_id, old_userentry_id) = reverse_map.get(comet).get
        if (old_room_id != room_id) {
          val old_comet_list = room_message_map.get(old_room_id) match {
            case Some(message_list) => message_list
            case _                  => List()
          }
          room_message_map.put(old_room_id, old_comet_list filterNot (_ == comet))
        }
        
        if (old_userentry_id != userentry_id) {
          userentry_message_map.remove(old_userentry_id)
        }
      }
      if (!comet_list.contains(comet))
        room_message_map.put(room_id, comet :: comet_list)
      userentry_message_map.put(userentry_id, comet)
      reverse_map.put(comet, (room_id, userentry_id))
    case RoomUnsubscribe(comet, room_id, userentry_id) =>
      val comet_list = room_message_map.get(room_id) match {
        case Some(message_list) => message_list
        case _                  => List()
      }
      room_message_map.put(room_id, comet_list filterNot (_ == comet))
      userentry_message_map.remove(userentry_id)
      reverse_map.remove(comet)
    case RoomDeleteRoom(room_id) =>
      room_message_map.remove(room_id)
      
    case NewMessage(room_id, talk) =>
      talk.save
      sendRoomMessage(room_id, in)
      RoomRound.find(By(RoomRound.room_id, room_id), OrderBy(RoomRound.round_no, Descending)) match {
        case Full(roomround) =>
          if (roomround.round_no.is == 0) {
            // 更新廢村時間
            val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is), OrderBy(RoomPhase.phase_no, Descending)).get
            roomphase.deadline(PlummUtil.dateAddMinute(new java.util.Date(), 10)).save
          }
        case xs => ;
      }
      
      //room.talk_time(new java.util.Date()).save
      
    case SignalAction(action) =>
      //val roomround_a = RoomRound.find(By(RoomRound.id, action.roomround_id.is)).get
      val actioner    = UserEntry.find(By(UserEntry.id, action.actioner_id.is)).get
      val room = Room.find(By(Room.id, actioner.room_id.is)).get
      val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
                                     OrderBy(RoomRound.round_no, Descending)).get
      val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
                                     OrderBy(RoomPhase.phase_no, Descending)).get
      val userentrys = UserEntry.findAllByRoom(room)
      val actioner_reload = userentrys.filter(_.id.is == actioner.id.is)(0)
      
      var enabled_action_list = ActionHelper.enabled_action_list(room, roomround, roomphase, actioner_reload, userentrys.filter(!_.revoked.is))
      action.save
      
      if (enabled_action_list.contains(ActionAttack)) {
        enabled_action_list = enabled_action_list ::: List(ActionMultiAttack, ActionNoAttack)
      }
      
      if (enabled_action_list.contains(ActionCardChoose)) {
        enabled_action_list = enabled_action_list ::: List(ActionDrawBlackCard, ActionDrawWhiteCard, ActionDrawGreenCard)
      }
        
      
      // 這裡加入檢核      
      if (enabled_action_list.map(_.action_enum.toString).contains(action.mtype.toString)) {
        val talk = Talk.create.roomround_id(action.roomround_id.is).mtype(action.mtype.is)
                              .actioner_id(action.actioner_id.is).actionee_id(action.actionee_id.is)
                              .message_flags(action.action_flags.is)
        talk.save
      
        sendRoomMessage(roomround.room_id.is, NewMessage(roomround.room_id.is, talk))
        ActionHelper.process_action(action, room, roomround, roomphase, actioner_reload, userentrys)
      } else {
        warn("Room : " + room)
        warn("RoomRound : " + roomround)
        warn("RoomPhase : " + roomphase)
        warn("Actioner : " + actioner_reload)
        warn("UserEntrys : " + userentrys)
        
        warn("Check Failed :  " )
        warn("Action : " + action.mtype.toString)
        warn("Enabled : " + enabled_action_list.map(_.action_enum.toString).mkString(","))
        
      }
    case SessionVarSet(room, roomround, roomphase, userentrys, card_list) =>
      sendRoomMessage(room.id.is, in)
      
    //case NewRoomRound(room_id, roomround) =>
    //  roomround.save
    //  sendRoomMessage(room_id, ForceUpdate(room_id, (List(ForceUpdateEnum.ACTION_BAR, ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.LOCATION_TABLE, ForceUpdateEnum.TALK_TABLE))))
    case RoomForceUpdate(room_id, updates) =>
      sendRoomMessage(room_id, in)
    case UserEntryForceUpdate(userentry_id, updates) =>
      sendUserEntryMessage(userentry_id, in)
      
    case TickPlayer(room_id, userentry_id, count_down) =>
      //println("RoomActor TickPlayer")  
      sendUserEntryMessage(userentry_id, in)
    case Timeout(room_id, userentry_id) => 
      //println("RoomActor Timeout")  
      //sendUserEntryMessage(userentry_id, in)
      val room = Room.find(By(Room.id, room_id)).get
      val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
                                     OrderBy(RoomRound.round_no, Descending)).get
      val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
                                     OrderBy(RoomPhase.phase_no, Descending)).get
      val userentrys = UserEntry.findAllByRoom(room)
      
      // 這裡加入檢核      
      if (roomphase.player.is == userentry_id) {
        if ((roomphase.phase_type.is != RoomPhaseEnum.GAMEHALL.toString) &&
            (roomphase.phase_type.is != RoomPhaseEnum.ENDED.toString)) {
          //println("RoomActor Process Timeout")
          
          var is_dead = false
          //var is_abandon = false
          
          val userentry = userentrys.filter(_.id.is == userentry_id)(0)
          if (userentry.hasnt_room_flag(UserEntryRoomFlagEnum.AUTOVOTED)) {
            userentry.add_room_flag(UserEntryRoomFlagEnum.AUTOVOTED)
            userentry.save
          } else {
            //userentry.location("").live(false)
            userentry.add_room_flag(UserEntryRoomFlagEnum.SKIPPED)
            userentry.save
            //is_dead = true
          }
          
          if (( // (roomphase.phase_type.is == RoomPhaseEnum.ATTACK.toString) &&
              ((userentry.has_item(CardEnum.B_MASAMUNE) ||
               (userentry.has_user_flag(UserEntryFlagEnum.TAUNT)))   )) ||
              (roomphase.phase_type.is == RoomPhaseEnum.CARD.toString) ||
              (roomphase.phase_type.is == RoomPhaseEnum.CARD_SKILL.toString) ||
              (roomphase.phase_type.is == RoomPhaseEnum.CARD_CHOOSE.toString)) {
            userentry.location("").live(false).add_room_flag(UserEntryRoomFlagEnum.SUDDENDEATH)
            is_dead = true
            
            val action = Action.create.roomround_id(roomround.id.is)
            
            val userentry_equips = userentry.items
            if (userentry_equips.length > 0) {
        
              // 剩下的裝備丟至墓地
              userentry.items.foreach { userentry_item =>
                val card = CardPool.find(By(CardPool.room_id, userentry.room_id.is),
                                         By(CardPool.card, userentry_item.card_enum.toString)).get
                card.owner_id(0).discarded(true).save
              }
              userentry.item_flags("")
            } 
            userentry.save   
            
            if (userentry.get_role == RoleDespair) {
              userentrys.foreach { userentry1 =>
                GameProcessor.check_item_victory(userentry1)
                if (userentry1.live.is && userentry1.has_user_flag(UserEntryFlagEnum.VICTORY2))
                  userentry1.add_user_flag(UserEntryFlagEnum.VICTORY).save
              }
            }
		  
            val live_daniels = userentrys.filter(x => (x.id.is != userentry.id.is) &&
              (!x.revoked.is) && (x.live.is) && (x.get_role == RoleDaniel) && (x.hasnt_user_flag(UserEntryFlagEnum.SEALED) && (!x.revealed.is)))
            live_daniels.foreach { live_daniel =>
              val action1 = Action.create.roomround_id(roomround.id.is).actioner_id(live_daniel.id.is)
                                         .mtype(MTypeEnum.ACTION_FLIP.toString)
              action1.save
              val talk = Talk.create.roomround_id(roomround.id.is).actioner_id(live_daniel.id.is)
                                    .mtype(MTypeEnum.ACTION_FLIP.toString)
              talk.save
              talk.send(live_daniel.room_id.is)
        
              if (live_daniel.damaged.is < 5)
                live_daniel.damaged(5)
              
              live_daniel.revealed(true)
              live_daniel.save
            }
            
            val live_cheshires = userentrys.filter(x => (x.get_role == RoleCheshire) && (x.live.is) &&
                                                 (x.target_user.is == userentry_id))
            live_cheshires.foreach { live_cheshire =>
            if (userentry.get_role.role_side == RoleSideEnum.NEUTRAL) {
              val saved_damaged = live_cheshire.damaged.is
              live_cheshire.damaged(99)
              GameProcessor.check_death(live_cheshire, live_cheshire, action, userentrys)
              live_cheshire.damaged(saved_damaged).save
            } else if (!live_cheshire.revealed.is)
              GameProcessor.flip(live_cheshire, action, userentrys)
            }
            
            val live_unrevealed = userentrys.filter(x => (x.get_role != RoleDetective) &&(x.live.is) && (!x.revealed.is))
            if (live_unrevealed.length == 0) {
              val live_detectives = userentrys.filter(x =>(x.get_role == RoleDetective) && (x.live.is))
              live_detectives.foreach { live_detective =>
                val saved_damaged = live_detective.damaged.is
                live_detective.damaged(99)
                GameProcessor.check_death(live_detective, live_detective, action, userentrys)
                live_detective.damaged(saved_damaged).save
              }
            }
          }  

          
          val talk = Talk.create.roomround_id(roomround.id.is).mtype(MTypeEnum.MESSAGE_TIMEOUT.toString)
                                .actioner_id(userentry_id)
          if (is_dead) 
            talk.mtype(MTypeEnum.MESSAGE_DEATHSUDDEN.toString)
          talk.save
          sendRoomMessage(roomround.room_id.is, NewMessage(roomround.room_id.is, talk))
          
          if ((is_dead) && (!userentry.revealed.is)) {
            val action = Action.create.roomround_id(roomround.id.is)
            GameProcessor.flip(userentry, action, userentrys)
          }
          
          val live_userentrys = userentrys.filter(x => (!x.revoked.is) && (x.live.is) && (x.hasnt_room_flag(UserEntryRoomFlagEnum.AUTOVOTED)))
          if (live_userentrys.length == 0) {
            GameProcessor.abandon(room)
            /*
            val new_roomround = RoomRound.create.room_id(room.id.is).round_no(roomround.round_no.is + 1)
                                         .last_round(roomround.id.is)
            new_roomround.save
            val talk = Talk.create.roomround_id(new_roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                     .message("遊戲結束 "+ (new java.util.Date).toString)
            talk.save
     
            val new_phase = RoomPhase.create.roomround_id(new_roomround.id.is).phase_no(0)
                                     .phase_type(RoomPhaseEnum.ENDED.toString)
            new_phase.save
             
            room.status(RoomStatusEnum.ENDED.toString).victory(RoomVictoryEnum.ABANDONED.toString)
            room.save
            
            RoomActor ! SessionVarSet(room = room, roomround = new_roomround, roomphase = new_phase)
            //RoomActor.sendRoomMessage(room_id, RoomForceUpdate(room_id ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR)))
            RoomActor.sendRoomMessage(room_id, RoomForceOut(room_id))
            */
          } else if (!GameProcessor.check_victory(room, roomround, userentrys))
            GameProcessor.next_player(room, roomround, roomphase, userentrys)
        }
      }
  }
}
