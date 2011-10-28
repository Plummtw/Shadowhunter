package org.plummtw.shadowhunter.comet

import _root_.net.liftweb._
import net.liftweb.mapper._
import http._
import SHtml._
import js._
import JsCmds._
import common._
import util._
import Helpers._
import JE.{JsRaw,Str}

import scala.xml.NodeSeq

import collection.mutable.{LinkedList, HashMap, SynchronizedMap}

import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.util._
import org.plummtw.shadowhunter.actor._
import org.plummtw.shadowhunter.data._
import org.plummtw.shadowhunter.heavy.ActionHelper
import org.plummtw.shadowhunter.heavy.CardHelper
import org.plummtw.shadowhunter.heavy.GameProcessor

import net.liftweb.http.js.jquery.JqJsCmds._


/*
case object CountdownTick
//case class ListenTo(actor: CometActor, auctions: List[Long])
//case class CurrentAuction(auction: Box[Auction])
case class NewTalk(talk : Talk)
case class NewTalkNewUser(talk : Talk)
//case class NewRoomRound(roomround : RoomRound)
//case object UserTableChanged
//case class RedirectException(val url : String) extends Exception
case class CometUpdate(updates : List[ForceUpdateEnum.Value])
*/

class GameComet extends CometActor with Logger {
  //private var talk_index = 0
  var saved_room_id : Long = 0
  var saved_userentry_id : Long = 0
  
  var room : Room = Room_R.get
  var roomround = RoomRound_R.get
  var roomphase = RoomPhase_R.get
  var currentuserentry : UserEntry = CurrentUserEntry_R.get
  var userentrys_rr = UserEntrys_RR.get
  
  var card_list : List[CardPool] = List()
  
  /*
  room = Room_E.get
  roomround = RoomRound_E.get
  roomphase = RoomPhase_E.get
  currentuserentry = CurrentUserEntry_E.get
  userentrys_rr = UserEntrys_ER.get
    
  Room_R.set(room)
  RoomRound_R.set(roomround)
  RoomPhase_R.set(roomphase)
  CurrentUserEntry_R.set(currentuserentry)
  UserEntrys_R.set(UserEntrys_E.get)
  UserEntrys_RR.set(userentrys_rr)
  */
  
  //override def lifespan = Full(120)
  //def slideDown(id : String, node : NodeSeq) : JsCmd = JsRaw("$('" + node.toString + "').hide().prependTo('#" + id + "').slideDown('slow')").cmd
  
  override def lowPriority = { 
    case NewMessage(room_id, talk) => { 
      if (room_id == saved_room_id) {
        //println("NewTalk received")
        partialUpdate(PrependHtml("talk-tbody", MessageHelper.talk_tag(talk, UserEntrys_R.get, reveal_mode))) 
      }
      //if(!hasExpired_?) Schedule.schedule(this, CountdownTick, 5 seconds) 
    }
    case SessionVarSet(new_room, new_roomround, new_roomphase, new_userentrys, new_card_list) =>
      if (new_room.id.is != saved_room_id)
        Noop
      else try {
        if (new_room != null) {
          room = new_room
          Room_R.set(room)
        }  
        if (new_roomround != null) {
          roomround = new_roomround
          RoomRound_R.set(roomround)
        }  
        if (new_roomphase != null) {
          roomphase = new_roomphase
          RoomPhase_R.set(roomphase)
        }  
        if (!new_userentrys.isEmpty) {
          currentuserentry = new_userentrys.find(_.id.is == saved_userentry_id).get
          CurrentUserEntry_R.set(currentuserentry)
          UserEntrys_R.set(new_userentrys)
          userentrys_rr = new_userentrys.filter(! _.revoked.is)
          UserEntrys_RR.set(userentrys_rr)
        }
        if (!new_card_list.isEmpty) {
          card_list = new_card_list
          CardPool_R.set(card_list)
        }
        
        // 檢測一下 Room Id 是否正確
        if (room.id.is != saved_room_id) {
          S.error(<b>Room Id檢核失敗</b>)
          partialUpdate(JsCmds.RedirectTo("main.html"))
        } else
          Noop
      } catch { case e: Exception =>
        e.printStackTrace
        S.error(<b>{e.toString}</b>)
        partialUpdate(JsCmds.RedirectTo("main.html"))
      }
    case RoomForceUpdate(room_id, updates) => {
      //println("RoomForceUpdate received")
      val cmd = 
        if (room_id != saved_room_id) {
          S.error(<b>Room Id檢核失敗</b>)
          JsCmds.RedirectTo("main.html")
        } else
          process_update(updates)
      partialUpdate(cmd)
    }
    case UserEntryForceUpdate(userentry_id, updates) => {
      //println("UserEntryForceUpdate received")
      val cmd = 
        if (userentry_id != saved_userentry_id) {
          S.error(<b>UserEntry Id檢核失敗</b>)
          JsCmds.RedirectTo("main.html")
        } else
          process_update(updates)
      partialUpdate(cmd)
    }
    case RoomForceOut(room_id) => {
      //println("RoomForceOut received")
      val cmd = 
        if (room_id != saved_room_id) {
          S.error(<b>Room Id檢核失敗</b>)
          JsCmds.RedirectTo("main.html")
        } else {
          S.error(<b>被強制踢出</b>)
          JsCmds.RedirectTo("main.html")
        }
      partialUpdate(cmd)
    }
    case ForceOut(userentry_id) => {
      //println("ForceOut received")
      val cmd = 
        if (userentry_id != saved_userentry_id) {
          S.error(<b>UserEntry Id檢核失敗</b>)
          JsCmds.RedirectTo("main.html")
        } else {
          S.error(<b>被強制踢出</b>)
          JsCmds.RedirectTo("main.html")
        }
      partialUpdate(cmd)
    }
    case ForceLogOut(userentry_id) => {
      //println("ForceLogOut received")
      val cmd = 
        if (userentry_id != saved_userentry_id) {
          S.error(<b>UserEntry Id檢核失敗</b>)
          JsCmds.RedirectTo("main.html")
        } else {
          CurrentRoom.set(Empty)
          CurrentUserEntry.set(Empty)
          S.error(<b>被強制踢出</b>)
          JsCmds.RedirectTo("main.html")
        }
      partialUpdate(cmd)
    }
    
    case TickPlayer(room_id, userentry_id, count_down) => {
      val cmd = 
        if (room_id != saved_room_id) {
          warn("TickPlayerFailed : room_id")
          S.error(<b>Room Id檢核失敗</b>)
          JsCmds.RedirectTo("main.html")
        } else if (userentry_id != saved_userentry_id) {
          warn("TickPlayerFailed : saved_userentry_id")
          S.error(<b>UserEntry Id檢核失敗</b>)
          JsCmds.RedirectTo("main.html")
        } else {
          //println("GameComet TickPlayer")  
          SetHtml("count_down", <span class="deadline">剩餘時間： {count_down.toString} 秒</span>)
        }
      partialUpdate(cmd)
    }
    
    /*
    case Timeout(room_id, userentry_id) => {
      val cmd = 
        if (room_id != saved_room_id) {
          S.error(<b>Room Id檢核失敗</b>)
          JsCmds.RedirectTo("main.html")
        } else if (userentry_id != saved_userentry_id) {
          S.error(<b>UserEntry Id檢核失敗</b>)
          JsCmds.RedirectTo("main.html")
        } else {
          val phase_type =
            try { RoomPhaseEnum.withName(roomphase.phase_type) }
            catch { case e : Exception => RoomPhaseEnum.NONE}
          phase_type match {
            case RoomPhaseEnum.MOVEMENT => ;
            case RoomPhaseEnum.LOCATION => ;
            case RoomPhaseEnum.ATTACK   => ;  
          }  
            
          Noop
        }
      partialUpdate(cmd)  
    }
    */
    /*
    case NewTalkNewUser(talk) => { 
      println("NewTalk received")
      val userentrys    = UserEntry.findAll(By(UserEntry.room_id, room.id.is))
      val userentrys_in = userentrys.filter(! _.revoked.is)
      UserEntrys_R.set(userentrys)
      UserEntrys_RR.set(userentrys_in)
      partialUpdate(PrependHtml("talk-tbody", MessageHelper.talk_tag(talk, false))) 
      //if(!hasExpired_?) Schedule.schedule(this, CountdownTick, 5 seconds) 
    }
    case CometUpdate(updates) => {
      println("CometUpdate received")
      var result : JsCmd = refresh_sessionvar
      if (result == Noop) {
        println("Noop")
        println("updates : " + updates.toString)
        if (updates.contains(ForceUpdateEnum.ACTION_BAR)) 
          result = result & SetHtml("action-bar", action_buttons)
        if (updates.contains(ForceUpdateEnum.USER_TABLE)) {
          println("UserEntrys_RR : " + UserEntrys_RR.get.toString )
          result = result & SetHtml("user-table", UserEntryHelper.user_table(Room_R.get.has_flag(RoomFlagEnum.TEST_MODE))) //UserEntryHelper.user_table(false)
        }  
        if (updates.contains(ForceUpdateEnum.LOCATION_TABLE)) 
          result = result & SetHtml("location-table", LocationHelper.location_table)
        if (updates.contains(ForceUpdateEnum.TALK_TABLE)) 
          result = result & SetHtml("talk-table", MessageHelper.messages_normal(Room_R.get.has_flag(RoomFlagEnum.TEST_MODE)))
        println(result)
      }
      partialUpdate(result)
    }
    */
  }

  def time_table =  {
    if (roomphase.phase_type.is == RoomPhaseEnum.GAMEHALL.toString)
      <span>遊戲大廳</span>
    else if (roomphase.phase_type.is == RoomPhaseEnum.ENDED.toString) {
      val role = currentuserentry.get_role
      val victory_roles = room.victory_all.is.split(",").map(RoleEnum.get_role(_))
      val victorys = room.victory_all.is.split(",").map(x => RoleEnum.get_role(x).role_name).mkString(",")

      val win_str =
        if (GameProcessor.check_user_victory(currentuserentry, room.victory.is))
          "你獲勝了"
        else if ((room.victory.is == RoomVictoryEnum.DRAW.toString) || (room.victory.is == RoomVictoryEnum.ABANDONED.toString))
          ""
        else if (victory_roles.contains(role))
          "你獲勝了"
        else
          "你已經輸了"
        
      if (room.victory_all.is == "")
        <span>遊戲結束，勝利者：{RoomVictoryEnum.victory_name(room.victory.is)}，{win_str}</span>
      else {
        <span>遊戲結束，勝利者：{RoomVictoryEnum.victory_name(room.victory.is)} {victorys}，{win_str}</span>
      }
    } else
      <span>第{roomround.round_no.is}日　等待 {userentrys_rr.filter(_.id.is == roomphase.player.is)(0).handle_name.is} {RoomPhaseEnum.get_cname(roomphase.phase_type.is)} <span id="count_down"></span></span>
  }    

  /*
  def action_div : List[ActionData] = {
    //roomphase = RoomPhase_R.get
    if (roomphase.phase_type.is == RoomPhaseEnum.GAMEHALL.toString)
      List(ActionKick, ActionStartGame) // ActionTestAlert
    else if (roomphase.phase_type.is == RoomPhaseEnum.ENDED.toString)
      List()
    else {
      // currentuserentry = CurrentUserEntry_R.get
      val role = currentuserentry.get_role
      if (roomphase.player.is != currentuserentry.id.is)
        role.free_skill :: List(ActionFlip, ActionCassandraGive)
      else if (roomphase.phase_type.is == RoomPhaseEnum.MOVEMENT.toString)
        role.free_skill :: role.movement_skill :: List(ActionFlip, ActionMove)
      else if (roomphase.phase_type.is == RoomPhaseEnum.LOCATION.toString)
        role.free_skill :: List(ActionFlip, ActionDrawBlackCard, ActionDrawWhiteCard, ActionDrawGreenCard, 
                                ActionLocDamage, ActionLocHeal, ActionLocRob, ActionNoLoc)
      else if ((roomphase.phase_type.is == RoomPhaseEnum.CARD.toString) ||
               (roomphase.phase_type.is == RoomPhaseEnum.CARD_SKILL.toString)){
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
        List(ActionFlip, ActionAttack, ActionNoAttack)
    }  
  }
  */
  
  def action_buttons = {
    ActionHelper.enabled_action_list(room, roomround, roomphase, 
                                     currentuserentry, userentrys_rr).map( x=>
      
      if ((Room_R.get.id.is == saved_room_id) &&
          (CurrentUserEntry_R.get.id.is == saved_userentry_id))
        x.toAjaxButton
      else 
        ajaxButton(this.toString, () => JsCmds.RedirectTo("main.html"))
    )
  }
  
  def render = {
    room = Room_E.get
    roomround = RoomRound_E.get
    roomphase = RoomPhase_E.get
    currentuserentry = CurrentUserEntry_E.get
    val userentrys = UserEntrys_E.get
    userentrys_rr = UserEntrys_ER.get
    
    Room_R.set(room)
    RoomRound_R.set(roomround)
    RoomPhase_R.set(roomphase)
    CurrentUserEntry_R.set(currentuserentry)
    UserEntrys_R.set(userentrys)
    UserEntrys_RR.set(userentrys_rr)
    
    saved_room_id = room.id.is
    saved_userentry_id = currentuserentry.id.is
    
    card_list = CardPool.findAll(By(CardPool.room_id, room.id.is), OrderBy(CardPool.card_no, Ascending))
    
    var say_text          = ""
    var font_type         = "normal"
    
    //println("Room :" + room)
    //println("roomround :" + roomround)
    //println("currentuserentry :" + currentuserentry)
    //println("userentrys :" + userentrys)
      
    RoomActor ! RoomSubscribe(this, saved_room_id, saved_userentry_id)
    //UserEntryActor ! UserEntrySubscribe(this, currentuserentry)
    
    def process_say = {
      //println("IN PROCESS SAY")
      //println("say_text : " + say_text)
      //println("font_type : " + font_type)
      
      val say_data = PlummUtil.encodeHtml(say_text, Talk.message.maxLen)
      
      if ((say_text != "") && List("large","slightlarge","normal","small").contains(font_type)) {
        val talk = Talk.create.roomround_id(roomround.id.is).actioner_id(currentuserentry.id.is)
                   .cssclass(font_type).message(say_data).mtype(MTypeEnum.TALK_DAY.toString)
                   
        val is_redundant = 
          ((currentuserentry.last_talk.is == say_data) &&
           ((new java.util.Date).before(PlummUtil.dateAddSecond(currentuserentry.updatedAt.is, 5))))
        
        if (!is_redundant) {
          currentuserentry.remove_room_flag(UserEntryRoomFlagEnum.SKIPPED)
          currentuserentry.last_talk(say_data)
          currentuserentry.save

          // 遊戲大廳時更新 room 的時間，用來判斷廢村用
          RoomActor ! NewMessage(saved_room_id, talk)

        }
      }
      
      SetValById("say","") & SetValById("font_type","normal")
    }
    
    def set_sound_link = 
      if (S.getSessionAttribute("sound").getOrElse("") != "on")
        a(() => set_sound(true), <span>[音效：關]</span>) 
      else
        a(() => set_sound(false), <span>[音效：開]</span>) 
    
    def set_sound(b : Boolean) : JsCmd = {
      S.setSessionAttribute("sound", if (b) "on" else "off")
      SetHtml("set_sound", set_sound_link)
    }
    
    def process_logout(is_force : Boolean) = {
      RoomActor ! RoomUnsubscribe(this, saved_room_id, saved_userentry_id)
      saved_room_id = 0
      saved_userentry_id = 0
      //theSession.destroySession
      val force_str = if (is_force) "&force=1" else ""
      JsCmds.RedirectTo("login.html?command=logout&room_no=" + room.id.is.toString + force_str)
    }
    
    val user_table = UserEntryHelper.user_table(room, roomphase, currentuserentry, userentrys_rr, reveal_mode)
    val location_table = LocationHelper.location_table(room, userentrys_rr)
    val talk_table = MessageHelper.messages_normal(room, roomround, userentrys, reveal_mode)
    val card_table = CardHelper.card_table(room, card_list)
    
    "#room_no"          #> room.id.is &
    "#room_name"        #> room.room_name.is &
    "#room_comment"     #> room.room_comment.is &
    "#logout_link"      #> a(() => process_logout(false), Seq(<span>[登出]</span>)) &
    "#logout_link2"     #> a(() => process_logout(true), Seq(<span>[登出2]</span>)) &
    "#set_sound *"      #> set_sound_link &
    // <a href={"login.html?command=logout&room_no=" + room.id.is.toString}>[登出]</a>
    "#go_out_link"      #> go_out_link &
    "name=font_type"    #> SHtml.select(Seq(("large","強力發言"),("slightlarge","稍強發言"),("normal","普通發言"),("small","小聲發言")),
                              Full(font_type),  x => font_type = x) &
    "name=say"          #> SHtml.textarea(say_text,     x => say_text = x) &
    "type=submit"       #> SHtml.ajaxSubmit("送出發言", () => process_say) &
    //"type=submit [onclick]"     #> SHtml.ajaxInvoke(() => process_say) &
    "#action-bar *"     #> action_buttons &
    "#time-table *"     #> time_table &
    "#user-table *"     #> user_table &
    "#card-table * "    #> card_table &
    "#location-table *" #> location_table &
    "#talk-table *"     #> talk_table 
  }
  //override def defaultPrefix = Full("clk")
  /*
​  def render = bind("time" -> timeSpan)
  def timeSpan = (<span id="time">{timeNow}</span>)
​  // schedule a ping every 10 seconds so we redraw
  ActorPing.schedule(this, Tick, 10000L)
​  override def lowPriority : PartialFunction[Any, Unit] = {
    case Tick => {
      println("Got tick " + new Date());
      partialUpdate(SetHtml("time", Text(timeNow.toString)))
      // schedule an update in 10 seconds
      ActorPing.schedule(this, Tick, 10000L)
    }
  }
  */
 
  /*
  def button(in: NodeSeq) =
  ajaxButton(in,
             () => S.runTemplate(List("_jsdialog_confirm")).
             map(ns => ModalDialog(ns)) openOr
             Alert("Couldn't find _jsdialog_confirm template"))

  // the template needs to bind to either server-side behavior
  // and unblock the UI
  def confirm(in: NodeSeq) =
  bind("confirm", in,
       "yes" -> ((b: NodeSeq) => ajaxButton(b, () =>
        {println("Rhode Island Destroyed")
         Unblock & Alert("Rhode Island Destroyed")})),
       "no" -> ((b: NodeSeq) => <button onclick={Unblock.toJsCmd}>{b}</button>))
 
  def alert(xhtml: NodeSeq): NodeSeq =
    Script(Alert("Important Alert Goes Here!"))
  */
  
  // val js: JsCmd = jsList.foldLeft[JsCmd](Noop)(_ & _)
  override def localShutdown() { 
    //if ((saved_room_id != 0) && (currentuserentry != null))
    RoomActor ! RoomUnsubscribe(this, saved_room_id, saved_userentry_id)
    saved_room_id = 0
    saved_userentry_id = 0
  }
  
  /*
  def refresh_sessionvar : JsCmd = {
    var result : JsCmd = Noop
    var room_box = Room.find(By(Room.id, Room_R.get.id.is)) 
    
    if (room_box.isEmpty) {
      S.error(<b>找不到村莊</b>)
      return JsCmds.RedirectTo("main.html")
    }
    
    val room = room_box.get
    CurrentRoom.set(Box !! room)
    Room_R.set(room)
    val room_id = room.id.is
    
    if (room.status.is == RoomStatusEnum.ENDED.toString()) {
      return JsCmds.RedirectTo("game_end.html?room_no=" + room_id)
    }
    
    val roomround_box  = RoomRound.find(By(RoomRound.room_id, room_id), OrderBy(RoomRound.round_no, Descending))
    if (roomround_box.isEmpty) {
      S.error(<b>找不到遊戲回合</b>)
      return JsCmds.RedirectTo("main.html")
    }
    
    val roomround = roomround_box.get
    RoomRound_R.set(roomround)
    
    val userentrys    = UserEntry.findAll(By(UserEntry.room_id, room_id))
    val userentrys_in = userentrys.filter(! _.revoked.is)
    
    val check_param   = S.attr("check").getOrElse("")
    
    // 檢查是否登入
    val currentuserentry_box = CurrentUserEntry.get
    if (currentuserentry_box.isEmpty) 
      return JsCmds.RedirectTo("game_view.html?room_no=" + room_id)
      
    var currentuserentry = currentuserentry_box.get
    
    val userentry_loaded_list = userentrys_in.filter(_.id.is == currentuserentry.id.is)
    if (userentry_loaded_list.length == 0)  
      return JsCmds.RedirectTo("game_view.html?room_no=" + room_id)
      
    
    val userentry_loaded = userentry_loaded_list(0)
    
    CurrentUserEntry.set(Box !! userentry_loaded)
    CurrentUserEntry_R.set(userentry_loaded)
    
    UserEntrys_R.set(userentrys)
    UserEntrys_RR.set(userentrys_in)
    
    //println("Checking --- End")
    result
  }
  */
 
  def go_out_link = 
    if (room.status.is == RoomStatusEnum.WAITING.toString)
      <a href={"login.html?command=go_out&room_no=" + room.id.is.toString}>[自刪]</a> 
    else <span/>
 
  def process_update(updates : List[ForceUpdateEnum.Value]) : JsCmd = {
    var result : JsCmd = Noop
    //println("updates : " + updates.toString)
    if (updates.contains(ForceUpdateEnum.GO_OUT_LINK)) 
      result = result & SetHtml("go_out_link", go_out_link)
    if (updates.contains(ForceUpdateEnum.ACTION_BAR)) {
      result = result & SetHtml("action-bar", action_buttons)
    } 
    if (updates.contains(ForceUpdateEnum.TIME_TABLE)) 
      result = result & SetHtml("time-table", time_table)
    if (updates.contains(ForceUpdateEnum.USER_TABLE)) {
      //println("UserEntrys_RR : " + UserEntrys_RR.get.toString )
      result = result & SetHtml("user-table", UserEntryHelper.user_table(room, roomphase, currentuserentry, userentrys_rr, reveal_mode)) //UserEntryHelper.user_table(false)
    }  
    if (updates.contains(ForceUpdateEnum.LOCATION_TABLE)) 
      result = result & SetHtml("location-table", LocationHelper.location_table(room, userentrys_rr))
    if (updates.contains(ForceUpdateEnum.TALK_TABLE)) 
      result = result & SetHtml("talk-table", MessageHelper.messages_normal(room, roomround, UserEntrys_R.get, reveal_mode))
    if (updates.contains(ForceUpdateEnum.CARD_TABLE)) 
      result = result & SetHtml("card-table", CardHelper.card_table(room, card_list))
    if (updates.contains(ForceUpdateEnum.SOUND)) {
      result = result & JsRaw("playSound(0);")
    }

    //println(result)
    result
  }
  
  def reveal_mode = {
    if (room == null)
      false
    else
      (room.has_flag(RoomFlagEnum.TEST_MODE) || (room.status.is == RoomStatusEnum.ENDED.toString))
  }
}
