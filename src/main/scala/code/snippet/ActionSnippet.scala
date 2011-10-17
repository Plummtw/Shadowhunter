package org.plummtw.shadowhunter.snippet

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


import scala.xml.NodeSeq

//import collection.mutable.{LinkedList, HashMap, SynchronizedMap}

import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.util._
import org.plummtw.shadowhunter.data._
import org.plummtw.shadowhunter.actor._
import org.plummtw.shadowhunter.actor._
import org.plummtw.shadowhunter.card._

import org.plummtw.shadowhunter.heavy.GameProcessor

class ActionSnippet extends Logger {
  val random = scala.util.Random
  
  /*
  def alert(in: NodeSeq) =
    ajaxButton("Test",
      () => S.runTemplate(List("dialog/test_dialog")).
      map(ns => ModalDialog(ns)) openOr
      Alert("Couldn't find _jsdialog_confirm template"))
  */
  
  def test(in: NodeSeq) =
    bind("confirm", in,
         "yes" -> ((b: NodeSeq) => ajaxButton(b, () =>
          {println("Rhode Island Destroyed")
          Unblock & Alert("Rhode Island Destroyed")})),
         "no" -> ((b: NodeSeq) => <button onclick={Unblock.toJsCmd}>{b}</button>))

  def kick(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get

    var target_str : String = ""
    val targets = ActionKick.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_KICK.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "kick" -> ajaxSubmit("踢出", () => { process; Unblock }),
         "cancel" -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def flip(in: NodeSeq) = {
    val roomround = RoomRound_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      
      ////println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_FLIP.toString)
                         .actioner_id(currentuserentry.id.is)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "flip" -> ajaxSubmit("翻開角色卡", () => { process; Unblock }),
         "cancel" -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def move(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    val targets = ActionMove.targetable_locations(room, roomround, roomphase, currentuserentry, userentrys_rr)
    var target_str : String = targets(0).toString
    val targets_sel = targets.map{x => (x.toString, LocationEnum.get_cname(x))}
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MOVE.toString)
                         .actioner_id(currentuserentry.id.is).action_flags(target_str)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "location_select" -> SHtml.select(targets_sel, Full(target_str), x => (target_str = x)),
         "move" -> ajaxSubmit("移動", () => { process; Unblock }),
         "cancel" -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def attack(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    //val currentuserentry = CurrentUserEntry_R.get
    val targets = ActionAttack.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      
      val action = 
      if ((currentuserentry.get_role == RoleDespair) && (currentuserentry.revealed.is) &&
          (currentuserentry.hasnt_user_flag(UserEntryFlagEnum.SEALED)))  
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MULTIATTACK.toString)
                               .actioner_id(currentuserentry.id.is)    
      else if (currentuserentry.has_item(CardEnum.B_MACHINE_GUN))
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_MULTIATTACK.toString)
                               .actioner_id(currentuserentry.id.is)
      else  
        Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ATTACK.toString)
                               .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                               
      RoomActor ! SignalAction(action)
    }
    
    def process_noattack = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_NOATTACK.toString)
                         .actioner_id(currentuserentry.id.is)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "attack"    -> ajaxSubmit("攻擊", () => { process; Unblock }),
         "no_attack" -> <span></span> /* (if (currentuserentry.hasnt_item(CardEnum.B_MASAMUNE))
           ajaxButton("放棄攻擊", () => { process_noattack; Unblock }) 
           else <span></span>) */ ,
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def locdamage(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionLocDamage.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LOCDAMAGE.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id)
      val actionee = userentrys_rr.filter(_.id.is == target_id)(0)
      if (actionee.has_user_flag(UserEntryFlagEnum.BARRIER))
        action.action_flags(UserEntryFlagEnum.BARRIER.toString)
      else if (actionee.has_item(CardEnum.W_FORTUNE_BROOCH))
        action.action_flags(CardEnum.W_FORTUNE_BROOCH.toString)
      else if ((room.has_flag(RoomFlagEnum.UNSEEN_RESIST) && (actionee.get_role == RoleUnseen) && (actionee.revealed.is) &&
               (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED))))
        action.action_flags(RoleEnum.UNSEEN.toString)
      
      
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "locdamage" -> ajaxSubmit("傷害", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def locheal(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionLocHeal.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LOCHEAL.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "locheal"   -> ajaxSubmit("治療", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def locrob(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionLocRob.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)

    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LOCROB.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "locrob"    -> ajaxSubmit("搶奪", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def locrob2(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionLocRob.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)

    var target_sel = CardEnum.EQUIPMENT_LIST.map(x=> (x.toString, CardEnum.get_card(x).card_name))
    var target_item_str = ""

    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_LOCROB.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id).action_flags(target_item_str)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "item_select" -> SHtml.select(target_sel, Full(target_item_str), x => (target_item_str = x)),
         "locrob"    -> ajaxSubmit("搶奪", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }

  
  def greencard(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionGreenCard.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val roomphase = RoomPhase_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_GREENCARD.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id).action_flags(roomphase.phase_flags.is)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "greencard" -> ajaxSubmit("使用綠卡", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def whitecard(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionWhiteCard.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val roomphase = RoomPhase_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_WHITECARD.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id).action_flags(roomphase.phase_flags.is)
      RoomActor ! SignalAction(action)
    }
    
    if (targets.length == 0) {
      warn("use_white_card : targets.length == 0")
      warn("room : " + room.id.is)
      warn("roomround : " + roomround.id.is)
      warn("roomphase : " + roomphase.id.is)
      warn("currentuserentry : " + currentuserentry.id.is)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "whitecard" -> ajaxSubmit("使用白卡", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def blackcard(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionBlackCard.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val roomphase = RoomPhase_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_BLACKCARD.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id).action_flags(roomphase.phase_flags.is)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "blackcard" -> ajaxSubmit("使用黑卡", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def cardchoose(in: NodeSeq) = {
    val room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    val (card1, card2) = GameProcessor.peek_card2(room, CardTypeEnum.withName(roomphase.phase_flags.is))
    
    val card_map  = Map(card1.id.is.toString->CardEnum.get_card(card1.card.is).card_name, 
                        card2.id.is.toString->CardEnum.get_card(card2.card.is).card_name)
    
    var card_id_str = card1.id.is.toString
    
    val card_radios = SHtml.radio(card_map.keys.toList, Full(card_id_str), x => card_id_str = x)
    
    def process = {
      val card = GameProcessor.draw_specific(room, card_id_str)
    
      if (CardEnum.get_card(card.card.is).isInstanceOf[Equipment]) 
        card.owner_id(currentuserentry.id.is)
      else
        card.discarded(true)
      card.save
      
      val mtype_enum = 
        if (roomphase.phase_flags.is == CardTypeEnum.BLACK.toString ) MTypeEnum.ACTION_DRAWBLACKCARD
        else if (roomphase.phase_flags.is == CardTypeEnum.WHITE.toString ) MTypeEnum.ACTION_DRAWWHITECARD
        else MTypeEnum.ACTION_DRAWGREENCARD
    
      val action = Action.create.roomround_id(roomround.id.is).actioner_id(currentuserentry.id.is)
                                 .mtype(mtype_enum.toString)
                                 .action_flags(card.card.is)
      RoomActor ! SignalAction(action)
      Noop
    }
    
    ajaxForm(bind("action", in,
         "cardchoose_1" -> <span>{card_radios(0)}<span>{CardEnum.get_card(card1.card.is).card_name}</span></span>,         
         "cardchoose_2" -> <span>{card_radios(1)}<span>{CardEnum.get_card(card2.card.is).card_name}</span></span>,
         "cardchoose" -> ajaxSubmit("選擇", () => { process; Unblock }),
         "cancel" -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def ray(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionUltrasoulRay.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ULTRASOUL_RAY.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "ray"      -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def uray(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionUltrasoulUray.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ULTRASOUL_URAY.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "uray"      -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"    -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def cursechain(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionEllenCurseChain.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ELLEN_CURSECHAIN.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "cursechain"        -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"            -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def lightning(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionFranklinLightning.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_FRANKLIN_LIGHTNING.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id).action_flags((random.nextInt(6)+1).toString)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "lightning"         -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"            -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def dynamiteheal(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionFukaDynamiteHeal.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_FUKA_DYNAMITEHEAL.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "dynamiteheal"      -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"            -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def demolish(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionGeorgeDemolish.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_GEORGE_DEMOLISH.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id).action_flags((random.nextInt(4)+1).toString)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "demolish"          -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"            -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def bloodfeast(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionCharlesBloodfeast.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_CHARLES_BLOODFEAST.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "bloodfeast"          -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"            -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def deceive(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = currentuserentry.role_flags.is
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_UNKNOWN_DECEIVE.toString)
                         .actioner_id(currentuserentry.id.is).action_flags(target_str)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "deceive_select" -> SHtml.select(RoleEnum.UNKNOWN_DECEIVE_LIST.map(x=>(x.toString, RoleEnum.get_role(x.toString).role_name)),
                              Full(target_str),  x => target_str = x),
         "deceive" -> ajaxSubmit("偽裝", () => { process; Unblock }),
         "cancel" -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def gravedig(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionDavidGravedig.targetable_cards(room, roomround, roomphase, currentuserentry, userentrys_rr)
    val targets_map = targets.map(x => (x.toString, CardEnum.get_card(x.toString).card_name))
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_DAVID_GRAVEDIG.toString)
                         .actioner_id(currentuserentry.id.is).action_flags(target_str)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "gravedig_select" -> SHtml.select(targets_map,
                              Full(targets_map(0)._1),  x => target_str = x),
         "gravedig" -> ajaxSubmit("盜墓", () => { process; Unblock }),
         "cancel" -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  
  def pray(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionFatherOconnelPray.targetable_cards(room, roomround, roomphase, currentuserentry, userentrys_rr)
    val targets_map = targets.map(x => (x.toString, CardEnum.get_card(x.toString).card_name))
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_FATHEROCONNEL_PRAY.toString)
                         .actioner_id(currentuserentry.id.is).action_flags(target_str)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "pray_select" -> SHtml.select(targets_map,
                              Full(targets_map(0)._1),  x => target_str = x),
         "pray"   -> ajaxSubmit("祈禱", () => { process; Unblock }),
         "cancel" -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def fatechange(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var targetuser_str : String = ""
    val targetusers = ActionCassandraFateChange.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)    
    
    var targetitem_str : String = ""
    val targetitems = currentuserentry.items
    val targets_map = targetitems.map(x => (x.card_enum.toString, x.card_name))    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        targetuser_str.toLong 
      } catch {case e: Exception => 0}

      
      //println("target_str1 : " + targetuser_str)
      //println("target_str2 : " + targetitem_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_CASSANDRA_FATECHANGE.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id).action_flags(targetitem_str)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
        "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targetusers, x => (targetuser_str = x)),
        "item_select" -> (if (targetitems.length > 0) SHtml.select(targets_map,
                              Full(targets_map(0)._1),  x => targetitem_str = x)
                          else <span></span>),
         "fatechange"-> (if (targetitems.length > 0) ajaxSubmit("給予", () => { process; Unblock })
                          else <span></span>),
                         
         "cancel" -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def cassandragive(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var targetuser_str : String = ""
    val targetusers = ActionCassandraGive.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)    
    
    var targetitem_str : String = ""
    val targetitems = currentuserentry.items
    val targets_map = targetitems.map(x => (x.card_enum.toString, x.card_name))
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        targetuser_str.toLong 
      } catch {case e: Exception => 0}

      
      //println("target_str1 : " + targetuser_str)
      //println("target_str2 : " + targetitem_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_CASSANDRA_GIVE.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id).action_flags(targetitem_str)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
        "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targetusers, x => (targetuser_str = x)),
        "item_select" -> 
          (if (targetitems.length > 0) SHtml.select(targets_map,
                              Full(targets_map(0)._1),  x => targetitem_str = x)
           else <span></span>),
         "cassandragive"-> 
           (if (targetitems.length > 0) ajaxSubmit("給予", () => { process; Unblock })
            else <span></span>),
         "cancel" -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def itempreferred(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    
    val targetitems = currentuserentry.items
    val targets_map = targetitems.map(x => (x.card_enum.toString, x.card_name)) ++ 
      Seq((PreferItem.card_enum.toString, PreferItem.card_name), 
          (PreferLife.card_enum.toString, PreferLife.card_name))
    var targetitem_str : String = 
      if (targets_map.map(_._1).contains(currentuserentry.item_preferred.is.toString))
        currentuserentry.item_preferred.is.toString
      else ""
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      
      //println("target_str : " + targetitem_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ITEMPREFERRED.toString)
                         .actioner_id(currentuserentry.id.is).action_flags(targetitem_str)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
        "itempreferred_select" -> SHtml.select(targets_map,
                              Full(targetitem_str),  x => targetitem_str = x),
        "itempreferred"-> ajaxSubmit("確定", () => { process; Unblock }),
        "cancel" -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def bomb(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    val targets = ActionBombBomb.targetable_locations(room, roomround, roomphase, currentuserentry, userentrys_rr)
    var target_str : String = targets(0).toString
    val targets_sel = targets.map{x => (x.toString, LocationEnum.get_cname(x))}
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_BOMB_BOMB.toString)
                         .actioner_id(currentuserentry.id.is).action_flags(target_str)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "location_select" -> SHtml.select(targets_sel, Full(target_str), x => (target_str = x)),
         "bomb" -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel" -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def reincarnate(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionAngelReincarnate.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ANGEL_REINCARNATE.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "reincarnate"       -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"            -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def braceup(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionEvanBraceup.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_EVAN_BRACEUP.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "braceup"         -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"            -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def kikou(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionFengKikou.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      val actionee = userentrys_rr.filter(_.id.is == target_id)(0)
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_FENG_KIKOU.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id)
                         .action_flags(if (actionee.items.length <= 1) "2" else "1")
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "kikou"             -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"            -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def taunt(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str : String = ""
    val targets = ActionADecoyTaunt.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_ADECOY_TAUNT.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str = x)),
         "taunt"             -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel"            -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def exchange(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_ids : List[Long] = List()
    val targets = ActionGodfatExchange.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    def process = {
      //val roomround = RoomRound_R.get
      //val currentuserentry = CurrentUserEntry_R.get
      /* val target_id : Long = try {
        target_str.toLong 
      } catch {case e: Exception => 0} */
      
      //println("target_id1 : " + target_ids(0).toString)
      //println("target_id2 : " + target_ids(1).toString)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_GODFAT_EXCHANGE.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_ids(0)).action_flags(target_ids(1).toString)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_choose_table(userentrys_rr, targets, x => (target_ids = target_ids ::: List(x))),
         "exchange"          -> ajaxSubmit("確定", () => {
           if (target_ids.length == 2) {
             process; Unblock 
           } else
             Unblock & Alert("選擇之玩家數不為 2")
         }),
         "cancel"            -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def reasona(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str1 : String = ""
    val targets = ActionDetectiveReasonA.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    var target_str2 : String = RoleSideEnum.SHADOW.toString
    
    
    def process = {
      val target_id : Long = try {
        target_str1.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_DETECTIVE_REASONA.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id).action_flags(target_str2)
      RoomActor ! SignalAction(action)
    }
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str1 = x)),
         "reasona_select" -> SHtml.select(RoleSideEnum.ROLESIDE_LIST.map(x=>(x.toString, RoleSideEnum.get_roleside_cname(x))),
                              Full(target_str2),  x => target_str2 = x),
         "reasona" -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel" -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
  
  def reasonr(in: NodeSeq) = {
    val room : Room = Room_R.get
    val roomround = RoomRound_R.get
    val roomphase = RoomPhase_R.get
    val currentuserentry : UserEntry = CurrentUserEntry_R.get
    val userentrys_rr = UserEntrys_RR.get
    
    var target_str1 : String = ""
    val targets = ActionDetectiveReasonR.targetable_users(room, roomround, roomphase, currentuserentry, userentrys_rr)
    
    var target_str2 : String = RoleEnum.UNKNOWN.toString
    
    
    def process = {
      val target_id : Long = try {
        target_str1.toLong 
      } catch {case e: Exception => 0}
      
      //println("target_str : " + target_str)
      val action = Action.create.roomround_id(roomround.id.is).mtype(MTypeEnum.ACTION_DETECTIVE_REASONR.toString)
                         .actioner_id(currentuserentry.id.is).actionee_id(target_id).action_flags(target_str2)
      RoomActor ! SignalAction(action)
    }
    
    val role_list = 
      if (roomround.round_no.is % 3 == 0)
        RoleEnum.NEUTRAL_LIST
      else
        RoleEnum.NO_NEUTRAL_LIST
    
    ajaxForm(bind("action", in,
         "user_select_table" -> UserEntryHelper.user_select_table(userentrys_rr, targets, x => (target_str1 = x)),
         "reasonr_select" -> SHtml.select(role_list.map(x=>(x.toString, RoleEnum.get_role(x).role_name)),
                              Full(target_str2),  x => target_str2 = x),
         "reasonr" -> ajaxSubmit("確定", () => { process; Unblock }),
         "cancel" -> <button onclick={Unblock.toJsCmd}>取消</button>))
  }
}