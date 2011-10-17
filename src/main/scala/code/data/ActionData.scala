package org.plummtw.shadowhunter.data

import _root_.net.liftweb._
import net.liftweb.mapper._
import http._
import SHtml._
import js._
import JsCmds._
import js.jquery._
import JqJsCmds._
import common._
import util._
import Helpers._
import actor._

//import scala.xml.NodeSeq

import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.actor._
import org.plummtw.shadowhunter.util._
import org.plummtw.shadowhunter.card._

import org.plummtw.shadowhunter.heavy.GameProcessor

trait UserEntryTargetable {
  def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    //val currentuserentry = CurrentUserEntry_R.get
    //val userentrys = UserEntrys_RR.get
    
    userentrys_rr.filter(x=>(x.id.is != currentuserentry.id.is) && (x.live.is))
  }  
}

trait LocationTargetable {
  def targetable_locations(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[LocationEnum.Value] = {
    val currentuserentry = CurrentUserEntry_R.get
    
    LocationEnum.LOCATION_LIST filterNot (_ == LocationEnum.get_loc(currentuserentry.location.is))
  }  
}

trait CardTargetable {
  def targetable_cards(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry])  : List[CardEnum.Value] = {
    //val userentrys = UserEntrys_RR.get
    
    List()
  }  
}

class ActionData(action: MTypeEnum.Value, str: String) {
  def action_enum     = action
  def tag_string      = str
  def command_name    = "action_" + action.toString
    
  def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : Boolean= true
  
  def js_command : JsCmd = Noop
  
  def js_dialog(dialog_name : String) = 
    S.runTemplate(List("dialog/" + dialog_name)).
      map(ns => ModalDialog(ns)) openOr
      Alert("Couldn't find " + dialog_name + " template")
      
  def js_action : JsCmd = {
    val roomround = RoomRound_R.get
    val currentuserentry = CurrentUserEntry_R.get
    
    val action = Action.create.roomround_id(roomround.id.is).actioner_id(currentuserentry.id.is)
                              .mtype(action_enum.toString)
    RoomActor ! SignalAction(action)
    Noop
  }
  
  def toAjaxButton = ajaxButton(this.toString, () => S.callOnce(js_command))

  override def toString(): String = tag_string // "[" + tag_string + "]"
}

object ActionNoAction extends ActionData(MTypeEnum.ACTION_NO_ACTION, "") {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry])  = false
}

object ActionTestAlert extends ActionData(MTypeEnum.ACTION_TEST_ALERT, "ALERT") {
  override def js_command : JsCmd =
    //Alert("Test")
    S.runTemplate(List("dialog/test_dialog")).
      map(ns => ModalDialog(ns)) openOr
      Alert("Couldn't find test_dialog template")
}

object ActionKick extends ActionData(MTypeEnum.ACTION_KICK, "踢人") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry])  = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.GAMEHALL.toString)
  }
  
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    //val roomround = RoomRound_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    //val userentrys = UserEntrys_RR.get
    
    val kick_actions = Action.findAll(By(Action.roomround_id, roomround.id.is),
                                      By(Action.mtype, MTypeEnum.ACTION_KICK.toString),
                                      By(Action.actioner_id, currentuserentry.id.is))
    val kick_actionees = kick_actions.map(_.actionee_id.is)
    
    userentrys_rr.filter(x=>(x.id.is != currentuserentry.id.is)
                         && (!kick_actionees.contains(x.id.is)))
  }

  override def js_command : JsCmd = js_dialog("kick_dialog")
}

object ActionStartGame extends ActionData(MTypeEnum.ACTION_STARTGAME, "開始遊戲") {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.GAMEHALL.toString) && 
    (currentuserentry.hasnt_room_flag(UserEntryRoomFlagEnum.VOTED))
  }
  
  override def js_command : JsCmd = js_action
}

object ActionFlip extends ActionData(MTypeEnum.ACTION_FLIP, "翻開")  {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    val role = currentuserentry.get_role
    
    //println("Flip Role : " + role.toString)
    //println("Role == UltraSoul : " + (role == RoleUltraSoul))
    //println("Role == Werewolf : " + (role == RoleWerewolf))
    //println("Role == Vampire : " + (role == RoleVampire))
    
    (roomphase.phase_type.is != RoomPhaseEnum.GAMEHALL.toString) &&
    (roomphase.phase_type.is != RoomPhaseEnum.ENDED.toString) &&
    ((role != RoleDaniel) || (currentuserentry.has_user_flag(UserEntryFlagEnum.SEALED))) &&
    (!currentuserentry.revealed) &&
    ((role != RoleUnknown) || (currentuserentry.damaged.is < 11))
  }
  
  override def js_command : JsCmd = js_dialog("flip_dialog")
}

object ActionMove extends ActionData(MTypeEnum.ACTION_MOVE, "移動") with LocationTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) &&
    (roomphase.player.is == currentuserentry.id.is)
  }
  
  override def js_command : JsCmd = js_dialog("move_dialog")
}

object ActionCardChoose extends ActionData(MTypeEnum.ACTION_CARDCHOOSE, "選擇卡片")  {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.LOCATION.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (roomphase.phase_flags.is != "")
  }
  
  override def js_command : JsCmd = js_dialog("cardchoose_dialog")
  
  /*
  override def targetable_cards(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry])  : List[CardEnum.Value] = {
    //val userentrys = UserEntrys_RR.get
    
    //CardEnum.EQUIPMENT_LIST
    val discarded_whites = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                            By(CardPool.card_type, CardTypeEnum.WHITE.toString),
                                            By(CardPool.discarded, true))
    
    discarded_whites.map(x=>CardEnum.get_card(x.card.is).card_enum)
                    .filter(x => !CardEnum.EQUIPMENT_LIST.contains(x))
  }
  */
}

object ActionDrawBlackCard extends ActionData(MTypeEnum.ACTION_DRAWBLACKCARD, "黑卡")  {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.LOCATION.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    ((currentuserentry.location.is == LocationEnum.UNDERWORLD_GATE.toString) ||
     (currentuserentry.location.is == LocationEnum.GRAVEYARD.toString))
  }
  
  override def js_command : JsCmd = {
    val room      = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    val targetuserentry  = userentrys_rr.filter(x => x.id.is == currentuserentry.target_user.is)
    
    val card_remains = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                        By(CardPool.discarded, false)) 
    val black_remains = card_remains.filter(x => 
                                            (x.card_no.is >= room.blackcard_index.is) &&
                                            (x.card_type.is ==  CardTypeEnum.BLACK.toString))
    
    if ((currentuserentry.get_role == RoleEvan) &&
        (targetuserentry.length != 0) && (targetuserentry(0).live.is) &&
        (black_remains.length != 1)) {
      roomphase.phase_flags(CardTypeEnum.BLACK.toString).save
      js_dialog("cardchoose_dialog")
      
    } else {
      val card = GameProcessor.draw_card(room, CardTypeEnum.BLACK)
    
      if (CardEnum.get_card(card.card.is).isInstanceOf[Equipment]) 
        card.owner_id(currentuserentry.id.is)
      else
        card.discarded(true)
      card.save
    
      val action = Action.create.roomround_id(roomround.id.is).actioner_id(currentuserentry.id.is)
                                 .mtype(MTypeEnum.ACTION_DRAWBLACKCARD.toString)
                                 .action_flags(card.card.is)
      RoomActor ! SignalAction(action)
      Noop
    }
  }
}

object ActionDrawWhiteCard extends ActionData(MTypeEnum.ACTION_DRAWWHITECARD, "白卡")  {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.LOCATION.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    ((currentuserentry.location.is == LocationEnum.UNDERWORLD_GATE.toString) ||
     (currentuserentry.location.is == LocationEnum.CHURCH.toString))
  }
  
  override def js_command : JsCmd = {
    val room      = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    val targetuserentry  = userentrys_rr.filter(x => x.id.is == currentuserentry.target_user.is)
    
    val card_remains = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                        By(CardPool.discarded, false)) 
    val white_remains = card_remains.filter(x => 
                                            (x.card_no.is >= room.whitecard_index.is) &&
                                            (x.card_type.is ==  CardTypeEnum.WHITE.toString))
    
    
    if ((currentuserentry.get_role == RoleEvan) &&
        (targetuserentry.length != 0) && (targetuserentry(0).live.is) &&
        (white_remains.length != 1)) {
      roomphase.phase_flags(CardTypeEnum.WHITE.toString).save
      js_dialog("cardchoose_dialog")
      
    } else {
      val card = GameProcessor.draw_card(room, CardTypeEnum.WHITE)
    
      if (CardEnum.get_card(card.card.is).isInstanceOf[Equipment]) 
        card.owner_id(currentuserentry.id.is)
      else
        card.discarded(true)
      card.save
    
      val action = Action.create.roomround_id(roomround.id.is).actioner_id(currentuserentry.id.is)
                                 .mtype(MTypeEnum.ACTION_DRAWWHITECARD.toString)
                                 .action_flags(card.card.is)
      RoomActor ! SignalAction(action)
      Noop
    }
  }

}

object ActionDrawGreenCard extends ActionData(MTypeEnum.ACTION_DRAWGREENCARD, "綠卡")  {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.LOCATION.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    ((currentuserentry.location.is == LocationEnum.UNDERWORLD_GATE.toString) ||
     (currentuserentry.location.is == LocationEnum.HERMIT_CABIN.toString))
  }

  override def js_command : JsCmd = {
    val room      = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    val targetuserentry  = userentrys_rr.filter(x => x.id.is == currentuserentry.target_user.is)
    
    val card_remains = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                        By(CardPool.discarded, false)) 
    val green_remains = card_remains.filter(x => 
                                            (x.card_no.is >= room.whitecard_index.is) &&
                                            (x.card_type.is ==  CardTypeEnum.GREEN.toString))
    
    if ((currentuserentry.get_role == RoleEvan) &&
        (targetuserentry.length != 0) && (targetuserentry(0).live.is) &&
        (green_remains.length != 1)) {
      roomphase.phase_flags(CardTypeEnum.GREEN.toString).save
      js_dialog("cardchoose_dialog")
    } else {
      val action = Action.create.roomround_id(roomround.id.is).actioner_id(currentuserentry.id.is)
                                .mtype(MTypeEnum.ACTION_DRAWGREENCARD.toString)
                                .action_flags(GameProcessor.draw_card(room, CardTypeEnum.GREEN).card.is)
      RoomActor ! SignalAction(action)
      Noop
    }
      
  }
}

object ActionNoLoc extends ActionData(MTypeEnum.ACTION_NOLOC, "放棄")  {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.LOCATION.toString) &&
    (roomphase.player.is == currentuserentry.id.is) 
  }
  
  override def js_command : JsCmd = js_action
}

object ActionLocDamage extends ActionData(MTypeEnum.ACTION_LOCDAMAGE, "傷害") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.LOCATION.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.location.is == LocationEnum.WIERD_WOODS.toString)
  }
  
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    //val userentrys = UserEntrys_RR.get
    userentrys_rr.filter(x=> (x.live.is) && ((currentuserentry.hasnt_user_flag(UserEntryFlagEnum.LOVER) || (x.hasnt_user_flag(UserEntryFlagEnum.LOVER)))))
  }
  
  override def js_command : JsCmd = js_dialog("locdamage_dialog")
}

object ActionLocHeal extends ActionData(MTypeEnum.ACTION_LOCHEAL, "治療") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.LOCATION.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.location.is == LocationEnum.WIERD_WOODS.toString)
  }
  
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    //val userentrys = UserEntrys_RR.get
    userentrys_rr.filter(x=> (x.live.is) )
  }  
  
  override def js_command : JsCmd = js_dialog("locheal_dialog")
}

object ActionLocRob extends ActionData(MTypeEnum.ACTION_LOCROB, "搶奪") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.LOCATION.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.location.is == LocationEnum.ERSTWHILE_ALTER.toString) 
  }
  
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    //val currentuserentry = CurrentUserEntry_R.get
    //val userentrys = UserEntrys_RR.get
    userentrys_rr.filter(x=> (x.live.is) && (x.id.is != currentuserentry.id.is) &&
                          (x.items.length > 0) && ((currentuserentry.hasnt_user_flag(UserEntryFlagEnum.LOVER) || (x.hasnt_user_flag(UserEntryFlagEnum.LOVER)))) )
  }  
  
  override def js_command : JsCmd = {
    val currentuserentry = CurrentUserEntry_R.get
    val role = currentuserentry.get_role
    
    if ((role == RoleBob) || (role == RoleDavid))
      js_dialog("locrob2_dialog")
    else
      js_dialog("locrob_dialog")
  }
}

object ActionAttack extends ActionData(MTypeEnum.ACTION_ATTACK, "攻擊") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.ATTACK.toString) &&
    (roomphase.player.is == currentuserentry.id.is)
    //(currentuserentry.hasnt_user_flag(UserEntryFlagEnum.FROG))
  }
  
  /*
  def targetable_users(room : Room, currentuserentry : UserEntry, userentrys : List[UserEntry]) : List[UserEntry] = {
   
    val neighbor = LocationHelper.neighbor(room, currentuserentry.location.is)
    
    val userentrys1 = userentrys.filter(x=>(x.id.is != currentuserentry.id.is) && (x.live.is) && (!x.revoked.is))

    if (currentuserentry.has_item(CardEnum.B_HANDGUN))
      userentrys1.filter(x => (x.location.is != neighbor) &&
                             ((x.location.is != currentuserentry.location.is)))
    else
      userentrys1.filter(x => (x.location.is == neighbor) ||
                             ((x.location.is == currentuserentry.location.is)))
  }
  */
  
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    //val room = Room_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    //val userentrys = UserEntrys_RR.get
    
    val neighbor = LocationHelper.neighbor(room, currentuserentry.location.is)
    
    val userentrys1 = userentrys_rr.filter(x=>(x.id.is != currentuserentry.id.is) && (x.live.is) && (!x.revoked.is) &&
      ((currentuserentry.hasnt_user_flag(UserEntryFlagEnum.LOVER) || (x.hasnt_user_flag(UserEntryFlagEnum.LOVER))))                                     
    )

    val userentrys2 =
      if ((currentuserentry.get_role == RoleDespair) && (currentuserentry.revealed.is) &&
          (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)))
        userentrys1
      else if (currentuserentry.has_item(CardEnum.B_HANDGUN))
        userentrys1.filter(x => (x.location.is != neighbor) &&
                               ((x.location.is != currentuserentry.location.is)))
      else
        userentrys1.filter(x => (x.location.is == neighbor) ||
                               ((x.location.is == currentuserentry.location.is)))
          
    val userentrys_decoys =
      userentrys2.filter(x => (x.revealed.is) && (x.get_role == RoleADecoy))
    
    if (currentuserentry.has_user_flag(UserEntryFlagEnum.FROG))
      List()
    else if (currentuserentry.has_user_flag(UserEntryFlagEnum.TAUNT) && (userentrys_decoys.length != 0))
      userentrys_decoys
    else if (userentrys2.length > 1)
      userentrys2.filterNot(x => (x.get_role == RoleUnseen) && (x.revealed.is) &&
                                 (x.hasnt_user_flag(UserEntryFlagEnum.SEALED)))
    else
      userentrys2
  }
  
  override def js_command : JsCmd = js_dialog("attack_dialog")
}

object ActionMultiAttack extends ActionData(MTypeEnum.ACTION_MULTIATTACK, "範圍攻擊") with UserEntryTargetable {
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    //val room = Room_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    //val userentrys = UserEntrys_RR.get
    
    val neighbor = LocationHelper.neighbor(room, currentuserentry.location.is)
    
    val userentrys1 = userentrys_rr.filter(x=>(x.id.is != currentuserentry.id.is) && (x.live.is) && (!x.revoked.is) &&
      ((currentuserentry.hasnt_user_flag(UserEntryFlagEnum.LOVER) || (x.hasnt_user_flag(UserEntryFlagEnum.LOVER))))                                     
    )

    val userentrys2 =
      if ((currentuserentry.get_role == RoleDespair) && (currentuserentry.revealed.is) &&
          (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)))
        userentrys1
      else if (currentuserentry.has_item(CardEnum.B_HANDGUN))
        userentrys1.filter(x => (x.location.is != neighbor) &&
                               ((x.location.is != currentuserentry.location.is)))
      else
        userentrys1.filter(x => (x.location.is == neighbor) ||
                               ((x.location.is == currentuserentry.location.is)))
          
    val userentrys_decoys =
      userentrys2.filter(x => (x.revealed.is) && (x.get_role == RoleADecoy))
    
    if (currentuserentry.has_user_flag(UserEntryFlagEnum.FROG))
      List()
    //else if (currentuserentry.has_user_flag(UserEntryFlagEnum.TAUNT) && (userentrys_decoys.length != 0))
    //  userentrys_decoys
    else if (userentrys2.length > 1)
      userentrys2.filterNot(x => (x.get_role == RoleUnseen) && (x.revealed.is) &&
                                 (x.hasnt_user_flag(UserEntryFlagEnum.SEALED)))
    else
      userentrys2
  }
}

object ActionNoAttack extends ActionData(MTypeEnum.ACTION_NOATTACK, "放棄攻擊")  {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    if ((ActionAttack.enabled(room, roomround, roomphase, currentuserentry, userentrys_rr)) && 
      (ActionAttack.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr).length == 0))
      true
    else if (currentuserentry.has_item(CardEnum.B_MASAMUNE) ||
             currentuserentry.has_user_flag(UserEntryFlagEnum.TAUNT))
      false
    else
      true
  }
  
  override def js_command : JsCmd = {
    val roomround = RoomRound_R.get
    val currentuserentry = CurrentUserEntry_R.get
    
    val action = Action.create.roomround_id(roomround.id.is).actioner_id(currentuserentry.id.is)
                              .mtype(MTypeEnum.ACTION_NOATTACK.toString)
    RoomActor ! SignalAction(action)
    Noop
  }
}

object ActionNextRound extends ActionData(MTypeEnum.ACTION_NEXTROUND, "結束回合")  {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.POST_ATTACK.toString) &&
    (roomphase.player.is == currentuserentry.id.is)
  }  
  
  override def js_command : JsCmd = js_action
}

object ActionGreenCard extends ActionData(MTypeEnum.ACTION_GREENCARD, "使用綠卡") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.CARD.toString) &&
    (roomphase.player.is == currentuserentry.id.is) 
  }
  
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    userentrys_rr.filter(x=>(x.id.is != currentuserentry.id.is) && (x.live.is) &&
	((currentuserentry.hasnt_user_flag(UserEntryFlagEnum.LOVER) || (x.hasnt_user_flag(UserEntryFlagEnum.LOVER)))))
  }
  
  override def js_command : JsCmd = js_dialog("greencard_dialog")
}

object ActionWhiteCard extends ActionData(MTypeEnum.ACTION_WHITECARD, "使用白卡") with UserEntryTargetable with Logger {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    ((roomphase.phase_type.is == RoomPhaseEnum.CARD_SKILL.toString) ||
     (roomphase.phase_type.is == RoomPhaseEnum.CARD.toString)) &&
    (roomphase.player.is == currentuserentry.id.is) 
  }

  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    //val room_phase = RoomPhase_R.get
    val card = CardEnum.get_card(roomphase.phase_flags.is)
    
    if (card.isInstanceOf[UserEntryTargetable])
      card.asInstanceOf[UserEntryTargetable].targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    else {
      warn("use_white_card : Not UserEntryTargetable")
      warn("card_enum : " + card.card_enum.toString)
      List()
    }
  }
  
  override def js_command : JsCmd = js_dialog("whitecard_dialog")
}

object ActionBlackCard extends ActionData(MTypeEnum.ACTION_BLACKCARD, "使用黑卡") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.CARD.toString) &&
    (roomphase.player.is == currentuserentry.id.is) 
  }
  
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    //val room_phase = RoomPhase_R.get
    val card = CardEnum.get_card(roomphase.phase_flags.is)
    
    if (card.isInstanceOf[UserEntryTargetable])
      card.asInstanceOf[UserEntryTargetable].targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    else
      List()
  }
  
  override def js_command : JsCmd = js_dialog("blackcard_dialog")
}

object ActionWerewolfAmbush extends ActionData(MTypeEnum.ACTION_WEREWOLF_AMBUSH, "伏擊")  {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    val role = currentuserentry.get_role
    
    (roomphase.phase_type.is != RoomPhaseEnum.GAMEHALL.toString) &&
    (roomphase.phase_type.is != RoomPhaseEnum.ENDED.toString) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (!currentuserentry.revealed)
  }
  
  override def js_command : JsCmd = js_action
}

object ActionAllieMotherLove extends ActionData(MTypeEnum.ACTION_ALLIE_MOTHERLOVE, "母愛")  {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    val role = currentuserentry.get_role
    (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (currentuserentry.revealed)
  }
  
  override def js_command : JsCmd = js_action
}

object ActionUltrasoulRay extends ActionData(MTypeEnum.ACTION_ULTRASOUL_RAY, "光線") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (room.has_flag(RoomFlagEnum.ULTRASOUL_RAY)) &&
    (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (currentuserentry.revealed) 
  }
  
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    //val currentuserentry = CurrentUserEntry_R.get
    //val userentrys = UserEntrys_RR.get
    
    userentrys_rr.filter(x=>(x.id.is != currentuserentry.id.is) && (x.live.is) &&
                           ((currentuserentry.hasnt_user_flag(UserEntryFlagEnum.LOVER) || (x.hasnt_user_flag(UserEntryFlagEnum.LOVER)))) )
  }
  
  override def js_command : JsCmd = js_dialog("ray_dialog")
}

object ActionUltrasoulUray extends ActionData(MTypeEnum.ACTION_ULTRASOUL_URAY, "究極光線") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (currentuserentry.revealed) 
  }
  
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    //val currentuserentry = CurrentUserEntry_R.get
    //val userentrys = UserEntrys_RR.get
    
    userentrys_rr.filter(x=>(x.id.is != currentuserentry.id.is) && (x.live.is)
                        && (x.location.is == LocationEnum.UNDERWORLD_GATE.toString) &&
                           ((currentuserentry.hasnt_user_flag(UserEntryFlagEnum.LOVER) || (x.hasnt_user_flag(UserEntryFlagEnum.LOVER)))) )
  }
  
  override def js_command : JsCmd = js_dialog("uray_dialog")
}

object ActionEllenCurseChain extends ActionData(MTypeEnum.ACTION_ELLEN_CURSECHAIN, "禁咒之鍊") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (currentuserentry.revealed)
  }
  
  /* override def targetable_users : List[UserEntry] = {
    val currentuserentry = CurrentUserEntry_R.get
    val userentrys = UserEntrys_RR.get
    
    userentrys.filter(x=>(x.id.is != currentuserentry.id.is) && (x.live.is))
  } */
  
  override def js_command : JsCmd = js_dialog("cursechain_dialog")
}

object ActionFranklinLightning extends ActionData(MTypeEnum.ACTION_FRANKLIN_LIGHTNING, "閃電") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (currentuserentry.revealed)
  }
  
  /* override def targetable_users : List[UserEntry] = {
    val currentuserentry = CurrentUserEntry_R.get
    val userentrys = UserEntrys_RR.get
    
    userentrys.filter(x=>(x.id.is != currentuserentry.id.is) && (x.live.is))
  } */
  
  override def js_command : JsCmd = js_dialog("lightning_dialog")
}

object ActionFukaDynamiteHeal extends ActionData(MTypeEnum.ACTION_FUKA_DYNAMITEHEAL, "爆破治療") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (currentuserentry.revealed)
  }
  
  /* override def targetable_users : List[UserEntry] = {
    val currentuserentry = CurrentUserEntry_R.get
    val userentrys = UserEntrys_RR.get
    
    userentrys.filter(x=>(x.id.is != currentuserentry.id.is) && (x.live.is))
  } */
 
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    //val currentuserentry = CurrentUserEntry_R.get
    //val userentrys = UserEntrys_RR.get
    userentrys_rr.filter(x=> (x.live.is))
  }  

  
  override def js_command : JsCmd = js_dialog("dynamiteheal_dialog")
}

object ActionGeorgeDemolish extends ActionData(MTypeEnum.ACTION_GEORGE_DEMOLISH, "粉碎") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (currentuserentry.revealed)
  }
  
  /* override def targetable_users : List[UserEntry] = {
    val currentuserentry = CurrentUserEntry_R.get
    val userentrys = UserEntrys_RR.get
    
    userentrys.filter(x=>(x.id.is != currentuserentry.id.is) && (x.live.is))
  } */
  
  override def js_command : JsCmd = js_dialog("demolish_dialog")
}

object ActionGregorBarrier extends ActionData(MTypeEnum.ACTION_GREGOR_BARRIER, "防護罩")  {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.POST_ATTACK.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (currentuserentry.revealed)
  }
  
  override def js_command : JsCmd = js_action
}

object ActionWightManipulate extends ActionData(MTypeEnum.ACTION_WIGHT_MANIPULATE, "操弄")  {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.POST_ATTACK.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (currentuserentry.revealed)
  }  
  
  override def js_command : JsCmd = {
    val roomround = RoomRound_R.get
    val currentuserentry = CurrentUserEntry_R.get
    val userentrys_length = UserEntrys_RR.get.length
    
    val dead_userentrys = UserEntrys_RR.get.filter(!_.live.is)
    val dead_userentrys_length = dead_userentrys.length
    
    val manipulate_length = 
      if (userentrys_length <=5)
        dead_userentrys_length + 1
      else if (userentrys_length >=9)
        math.max(dead_userentrys_length - 1, 0)
      else
        dead_userentrys_length
    
    val action = Action.create.roomround_id(roomround.id.is).actioner_id(currentuserentry.id.is)
                              .mtype(action_enum.toString).action_flags(manipulate_length.toString)
    RoomActor ! SignalAction(action)
    Noop
  }
}

object ActionCharlesBloodfeast extends ActionData(MTypeEnum.ACTION_CHARLES_BLOODFEAST, "血祭") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.POST_ATTACK.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.damaged.is < 9) && 
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (currentuserentry.revealed)
  }  

  override def js_command : JsCmd = js_dialog("bloodfeast_dialog")
}

//object ActionVengefulGhostCurse extends ActionData(MTypeEnum.ACTION_VENGEFULGHOST_CURSE, "")

object ActionUnknownDeceive extends ActionData(MTypeEnum.ACTION_UNKNOWN_DECEIVE, "偽裝") {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    (roomphase.phase_type.is != RoomPhaseEnum.GAMEHALL.toString) &&
    (roomphase.phase_type.is != RoomPhaseEnum.ENDED.toString) &&
    (!currentuserentry.revealed)
  }
  
  override def js_command : JsCmd = js_dialog("deceive_dialog")
}

object ActionDavidGravedig extends ActionData(MTypeEnum.ACTION_DAVID_GRAVEDIG, "盜墓") with CardTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    (roomphase.phase_type.is != RoomPhaseEnum.GAMEHALL.toString) &&
    (roomphase.phase_type.is != RoomPhaseEnum.ENDED.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (currentuserentry.revealed)
  }
  
  override def js_command : JsCmd = js_dialog("gravedig_dialog")
  
  override def targetable_cards(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry])  : List[CardEnum.Value] = {
    //val userentrys = UserEntrys_RR.get
    
    //CardEnum.EQUIPMENT_LIST
    //userentrys_rr.filter(x=>x.id.is!=currentuserentry.id.is).map(_.items.map(_.card_enum)).flatten
         //.filter(x => CardEnum.EQUIPMENT_LIST.contains(x))
    val discarded_cards = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                           // By(CardPool.owner_id, 0),
                                           By(CardPool.discarded, true))
    
    discarded_cards.map(x=>CardEnum.get_card(x.card.is).card_enum)
                   .filter(x => CardEnum.EQUIPMENT_LIST.contains(x))
  } 
}

object ActionFatherOconnelPray extends ActionData(MTypeEnum.ACTION_FATHEROCONNEL_PRAY, "祈禱") with CardTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    (roomphase.phase_type.is != RoomPhaseEnum.GAMEHALL.toString) &&
    (roomphase.phase_type.is != RoomPhaseEnum.ENDED.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (currentuserentry.revealed)
  }
  
  override def js_command : JsCmd = js_dialog("pray_dialog")
  
  override def targetable_cards(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry])  : List[CardEnum.Value] = {
    //val userentrys = UserEntrys_RR.get
    
    //CardEnum.EQUIPMENT_LIST
    val discarded_whites = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                            By(CardPool.card_type, CardTypeEnum.WHITE.toString),
                                            By(CardPool.discarded, true))
    
    discarded_whites.map(x=>CardEnum.get_card(x.card.is).card_enum)
                    .filter(x => !CardEnum.EQUIPMENT_LIST.contains(x))
  } 
}

object ActionCassandraFateChange extends ActionData(MTypeEnum.ACTION_CASSANDRA_FATECHANGE, "命運轉變") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.player.is == currentuserentry.id.is) &&
    (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)) &&
    (currentuserentry.revealed) &&
    (currentuserentry.items.length != 0)
  }  

  override def js_command : JsCmd = js_dialog("fatechange_dialog")
}

object ActionCassandraGive extends ActionData(MTypeEnum.ACTION_CASSANDRA_GIVE, "給予") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val live_cassandras = userentrys_rr.filter(x => (x.live.is) && (x.revealed.is))
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    //(live_cassandras.length != 0) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) &&
    (currentuserentry.get_role != RoleCassandra) &&
    (currentuserentry.items.length != 0)
  }  
  
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    userentrys_rr.filter(x=>(x.id.is != currentuserentry.id.is) && (x.live.is) &&
                            (x.revealed.is) && (x.get_role == RoleCassandra))
  }  
  override def js_command : JsCmd = js_dialog("cassandragive_dialog")
}

object ActionItemPreferred extends ActionData(MTypeEnum.ACTION_ITEMPREFERRED, "綠卡設定") {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val role = currentuserentry.get_role
    (roomphase.phase_type.is != RoomPhaseEnum.GAMEHALL.toString) &&
    (roomphase.phase_type.is != RoomPhaseEnum.ENDED.toString)
  }
  
  override def js_command : JsCmd = js_dialog("itempreferred_dialog")
}

object ActionBombBomb extends ActionData(MTypeEnum.ACTION_BOMB_BOMB, "炸彈") with LocationTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)) &&
    (currentuserentry.revealed)
  }
  
  override def targetable_locations(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[LocationEnum.Value] = 
    LocationEnum.LOCATION_LIST
  
  override def js_command : JsCmd = js_dialog("bomb_dialog")
}

object ActionAngelReincarnate extends ActionData(MTypeEnum.ACTION_ANGEL_REINCARNATE, "重生選擇") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    (room.has_flag(RoomFlagEnum.ANGEL_CHOOSE)) &&
    (roomphase.phase_type.is != RoomPhaseEnum.GAMEHALL.toString) &&
    (roomphase.phase_type.is != RoomPhaseEnum.ENDED.toString) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED))
  }
  
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    userentrys_rr.filter(x=>(x.id.is != currentuserentry.id.is) && (!x.live.is) &&
                            (x.get_role != RoleAngel))
  }
  
  
  override def js_command : JsCmd = js_dialog("reincarnate_dialog")
}

object ActionEvanBraceup extends ActionData(MTypeEnum.ACTION_EVAN_BRACEUP, "振作") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (currentuserentry.revealed) &&
    (userentrys_rr.filter(x => (x.has_user_flag(UserEntryFlagEnum.LOVER)) && (x.live.is)).length <= 1)
  }
  
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    //val currentuserentry = CurrentUserEntry_R.get
    //val userentrys = UserEntrys_RR.get
    
    userentrys_rr.filter(x=>(x.id.is != currentuserentry.id.is) && (x.live.is) &&
                         (x.has_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)))
  }  
  
  override def js_command : JsCmd = js_dialog("braceup_dialog")
}

object ActionFengKikou extends ActionData(MTypeEnum.ACTION_FENG_KIKOU, "氣功") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.ATTACK.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.FROG)) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.TAUNT)) &&
    (currentuserentry.revealed) &&
    (currentuserentry.items.length == 0)
  }
  
  override def js_command : JsCmd = js_dialog("kikou_dialog")
}

object ActionADecoyTaunt extends ActionData(MTypeEnum.ACTION_ADECOY_TAUNT, "嘲諷") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)) &&
    (currentuserentry.revealed) 
  }
  
  override def js_command : JsCmd = js_dialog("taunt_dialog")
}

object ActionGodfatExchange extends ActionData(MTypeEnum.ACTION_GODFAT_EXCHANGE, "交換位置") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_SKILL_USED)) &&
    (currentuserentry.revealed) 
  }
  
  override def js_command : JsCmd = js_dialog("exchange_dialog")
  
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    //val currentuserentry = CurrentUserEntry_R.get
    //val userentrys = UserEntrys_RR.get
    
    userentrys_rr.filter(x=> (x.live.is))
  }  
}

object ActionDetectiveReasonA extends ActionData(MTypeEnum.ACTION_DETECTIVE_REASONA, "推理陣營") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomround.round_no.is != 1) &&
    (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED))
  }
  
  override def js_command : JsCmd = js_dialog("reasona_dialog")
  
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    //val currentuserentry = CurrentUserEntry_R.get
    //val userentrys = UserEntrys_RR.get
    
    userentrys_rr.filter(x=> (x.id.is != currentuserentry.id.is) && (x.live.is) && (!x.revealed.is) &&
                         (x.hasnt_user_flag(UserEntryFlagEnum.REASONAED)))
  }  
}

object ActionDetectiveReasonR extends ActionData(MTypeEnum.ACTION_DETECTIVE_REASONR, "推理角色") with UserEntryTargetable {
  override def enabled(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) = {
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    (roomround.round_no.is != 1) &&
    (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString) &&
    (roomphase.player.is == currentuserentry.id.is) &&
    (currentuserentry.hasnt_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED))
  }
  
  override def js_command : JsCmd = js_dialog("reasonr_dialog")
  
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    //val currentuserentry = CurrentUserEntry_R.get
    //val userentrys = UserEntrys_RR.get
    
    userentrys_rr.filter(x=> (x.id.is != currentuserentry.id.is) && (x.live.is) && (!x.revealed.is))
  }  
}

