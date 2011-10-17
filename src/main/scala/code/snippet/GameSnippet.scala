package org.plummtw.shadowhunter.snippet

import _root_.net.liftweb._
import net.liftweb.mapper._
import http._
import SHtml._
import js._
import JsCmds._
import common._
import util._
import Helpers._

import scala.xml.NodeSeq

import collection.mutable.{LinkedList, HashMap, SynchronizedMap}

import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.util._
import org.plummtw.shadowhunter.actor._
import org.plummtw.shadowhunter.heavy.CardHelper

object AutoReload extends SessionVar[Box[String]](Box !! "")

// 村莊處理用 Lock
object GameProcessLock {
  val lock_hash = new HashMap[Long, Object] with SynchronizedMap[Long, Object]
  
  // 每個 村莊 給一個 Lock
  def get_lock(id: Long) : Object = {
    if (!lock_hash.contains(id))
      lock_hash(id) = new Object()
    lock_hash(id)
  }
  
  def remove_lock(id: Long) = 
    lock_hash -= id
}

class GameCheckSnippet {
  def render = {
    val room_no = 
      try { S.param("room_no").getOrElse("0").toLong }
      catch { case e:Exception => 0}
    var room_box = Room.find(By(Room.id, room_no)) 
    if (room_box.isEmpty) {
      for (room <- CurrentRoom.get) {
        // Reload Room
        room_box = Room.find(By(Room.id, room.id.is))
      }
    }
    
    if (room_box.isEmpty) {
      S.error(<b>找不到村莊</b>)
      S.redirectTo("main.html")
    }
    
    val room = room_box.get
    CurrentRoom.set(Box !! room)
    Room_E.set(room)
    val room_id = room.id.is
    
    //if (room.status.is == RoomStatusEnum.ENDED.toString()) {
    //  S.redirectTo("game_end.html?room_no=" + room_id)
    //}
    
    val roomround_box  = RoomRound.find(By(RoomRound.room_id, room_id), OrderBy(RoomRound.round_no, Descending))
    if (roomround_box.isEmpty) {
      S.error(<b>找不到遊戲回合</b>)
      S.redirectTo("main.html")
    }
    
    val roomround = roomround_box.get
    //println("RoomRound ID : " + roomround.id.is)
    RoomRound_E.set(roomround)
    
    val roomphase_box  = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is), OrderBy(RoomPhase.phase_no, Descending))
    if (roomphase_box.isEmpty) {
      S.error(<b>找不到遊戲階段</b>)
      S.redirectTo("main.html")
    }
    
    val roomphase = roomphase_box.get
    RoomPhase_E.set(roomphase)
    
    val userentrys    = UserEntry.findAllByRoom(room)
    val userentrys_in = userentrys.filter(! _.revoked.is)
    
    ClockActor ! SessionVarSet(room, roomround, roomphase, userentrys)
    
    val check_param   = S.attr("check").getOrElse("")
    
    // 檢查是否登入
    if (check_param == "view")
      for (currentuserentry <- CurrentUserEntry.get;
           userentry_loaded <- userentrys_in.filter(_.id.is == currentuserentry.id.is)) {
        if (!userentry_loaded.revoked) 
          S.redirectTo("game.html?room_no=" + room_id)
        
        CurrentUserEntry_E.set(currentuserentry)
      }
    else if (check_param == "game") {
      val currentuserentry_box = CurrentUserEntry.get
      if (currentuserentry_box.isEmpty) 
        S.redirectTo("game_view.html?room_no=" + room_id)
      
      var currentuserentry = currentuserentry_box.get
    
      val userentry_loaded_list = userentrys_in.filter(_.id.is == currentuserentry.id.is)
      if (userentry_loaded_list.length == 0)  
        S.redirectTo("game_view.html?room_no=" + room_id)
      
    
      val userentry_loaded = userentry_loaded_list(0)
      //if (userentry_loaded.revoked) 
      //  S.redirectTo("game.html?room_no=" + room_id)
    
      CurrentUserEntry.set(Box !! userentry_loaded)
      CurrentUserEntry_E.set(userentry_loaded)
    }
    
    UserEntrys_E.set(userentrys)
    UserEntrys_ER.set(userentrys_in)
    
    //println("Checking --- End")
    NodeSeq.Empty
  }
}

class GameViewSnippet {
  def render = {
    val room = Room_E.get
    val roomround = RoomRound_E.get
    val roomphase = RoomPhase_E.get
    val currentuserentry = CurrentUserEntry_E.get
    val userentrys1 = UserEntrys_E.get
    val userentrys  = UserEntrys_ER.get
    
    /*
    Room_R.set(room)
    RoomRound_R.set(roomround)
    RoomPhase_R.set(roomphase)
    CurrentUserEntry_R.set(currentuserentry)
    UserEntrys_R.set(userentrys1)
    UserEntrys_RR.set(userentrys)
    */
    
    val room_no = room.id.is
    
    // 是否更新 Session 內容
    val auto_reload = S.param("auto_reload")
    val auto_reload_str = auto_reload match {
      case xs if (roomphase.phase_type == RoomPhaseEnum.ENDED.toString) =>
        AutoReload.set(Box !! "")   ; ""
      case Full("15") => AutoReload.set(Box !! "15") ; "15"
      case Full("0")  => AutoReload.set(Box !! "")   ; ""
      case _          => AutoReload.get.getOrElse("") ; ""
    }
    
    val auto_reload_seq : NodeSeq = Seq(
       if (auto_reload_str == "") <span>手動</span>
       else <a href={"game_view.html?room_no=" + room_no + "&auto_reload=0"}>手動</a>,
       if (auto_reload_str == "15") <span>15秒</span>
       else <a href={"game_view.html?room_no=" + room_no + "&auto_reload=15"}>15秒</a>)
       
    val time_table = 
      if (roomphase.phase_type.is == RoomPhaseEnum.GAMEHALL.toString)
        <span>遊戲大廳</span>
      else if (roomphase.phase_type.is == RoomPhaseEnum.ENDED.toString) {
        if (room.victory_all.is == "")
          <span>遊戲結束，勝利者：{RoomVictoryEnum.victory_name(room.victory.is)}</span>
        else {
          val victorys = room.victory_all.is.split(",").map(RoleEnum.get_role(_).role_name).mkString(",")
          <span>遊戲結束，勝利者：{RoomVictoryEnum.victory_name(room.victory.is)} {victorys}</span>
        }
      } else
        <span>第{roomround.round_no.is}日　等待 {UserEntry.get(roomphase.player.is, userentrys).handle_name.is} {RoomPhaseEnum.get_cname(roomphase.phase_type.is)}</span>
     val user_table = UserEntryHelper.user_table(room, roomphase, GlobalUserEntry.NoUserEntry, userentrys, reveal_mode)
    val location_table = LocationHelper.location_table(room, userentrys)
    val talk_table = MessageHelper.messages_normal(room, roomround, userentrys1, reveal_mode)
    val card_table = CardHelper.card_table(room, CardPool.findAll(By(CardPool.room_id, room.id.is), OrderBy(CardPool.card_no, Ascending)))
    
    //<meta http-equiv="refresh" content={auto_reload_str} />
    "@refresh [content]" #> auto_reload_str &
    "#room_no"        #> room_no &
    "#room_name"      #> room.room_name.is &
    "#room_comment"   #> room.room_comment.is &
    "name=room_no [value]" #> room_no &
    "#refresh [href]" #> ("game_view.html?room_no=" + room_no) &
    "#refresh-list"   #> auto_reload_seq &
    "#room-register" #> (if (room.status.is == RoomStatusEnum.WAITING.toString)
                        <a href={"room_register.html?room_no=" + room_no}>[住民登錄]</a> 
                      else <span/>) &
    "#time-table *"      #> time_table &
    "#user-table *"      #> user_table &
    "#card-table *"      #> card_table &
    "#location-table  *" #> location_table &
    "#talk-table *"      #> talk_table
  }
  
  def reveal_mode =
    (Room_E.get.has_flag(RoomFlagEnum.TEST_MODE) || (Room_E.get.status.is == RoomStatusEnum.ENDED.toString))  
}
