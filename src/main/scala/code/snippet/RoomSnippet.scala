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

import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.util._

class RoomListSnippet {
  def render(in : NodeSeq) = {
    val room_list  = Room.findAll(By(Room.status, RoomStatusEnum.WAITING.toString), OrderBy(Room.id,Descending)) :::
                     Room.findAll(By(Room.status, RoomStatusEnum.PLAYING.toString), OrderBy(Room.id,Descending))
                     
  
    room_list.flatMap { room => Seq(
      <a href={"login.html?room_no=" + room.id}>
      {
        if (room.status.is == RoomStatusEnum.WAITING.toString)
          <img class="option" src="images/waiting.gif" alt="等待中" title="等待中"/>
        else
          <img class="option" src="images/playing.gif" alt="進行中" title="進行中"/>
      }
      <span> [{room.id}號村] </span>{room.room_name}村<br/><div class="room_comment">～ {room.room_comment} ～
      [移動:{room.move_time}][行動:{room.action_time}][反應:{room.reaction_time}]
      [ {room.max_user}人用 ] </div></a>, 
      <div class="room_option">村莊選項：{room.option_text}</div>, <br/>) }
  }
}

// 創新村莊的 Lock，以免村莊數超過村莊上限
object RoomCreateLock {}

class RoomCreateSnippet extends StatefulSnippet with Logger{
  private var room_name        = ""
  private var room_comment     = ""
  private var max_user         = 10
  private var move_time        = 120
  private var action_time      = 115
  private var reaction_time    = 125
  
  def dispatch = {
    case _ => render
  }

  def render =
  {
    var option_list : List[RoomFlagEnum.Value] = List()

    def process() {
      debug("In Process")
      
      val room_flags : String= option_list.distinct.map(_.toString).mkString("",",","")
      
      val room = Room.create.room_name(room_name.replace('　',' ').trim()).room_comment(room_comment.replace('　',' ').trim()).max_user(max_user)
                     .move_time(move_time).action_time(action_time).reaction_time(reaction_time)
                     .room_flags(room_flags).status(RoomStatusEnum.WAITING.toString).victory("")
                     
      room.validate match {
        case Nil => ;
        case xs  => S.error(xs); return redirectTo("main.html")
      }
      
      // 加入大廳
      val game_hall = RoomRound.create.round_no(0)
      
      val room_params = AdminManage.findAll(Like(AdminManage.param_name, "room%"))

      val current_time =  new java.util.GregorianCalendar
      val current_hour =  current_time.get(java.util.Calendar.HOUR_OF_DAY)

      val room_start =
          try { room_params.filter(_.param_name.is == "room_start")(0).param_value.is.toInt }
          catch { case e: Exception => AdminHelper.DEFAULT_ROOM_START}
      val room_end =
          try { room_params.filter(_.param_name.is == "room_end")(0).param_value.is.toInt }
          catch { case e: Exception => AdminHelper.DEFAULT_ROOM_END}

      if ((current_hour >= room_end) || (current_hour< room_start)) {
        S.error((room_end.toString) + ":00 - " + (room_start.toString) +":00 請不要開村")
        return redirectTo("main.html")
      }
      
      RoomCreateLock.synchronized {
      
        val room_count  = Room.count(By(Room.status, RoomStatusEnum.WAITING.toString)) +
                          Room.count(By(Room.status, RoomStatusEnum.PLAYING.toString))
        val room_count_limit =
          try { room_params.filter(_.param_name.is == "room_count")(0).param_value.is.toInt }
          catch { case e: Exception => AdminHelper.DEFAULT_ROOM_COUNT}

        if (room_count >= room_count_limit) {
          S.error("超過村數上限"); return redirectTo("main.html")
        }
                          
        room.save

        game_hall.room_id(room.id.is)
        game_hall.save
        
        val room_phase = RoomPhase.create.roomround_id(game_hall.id.is).phase_no(0).phase_type(RoomPhaseEnum.GAMEHALL.toString)
        room_phase.deadline(PlummUtil.dateAddMinute(new java.util.Date(), 10))
        
        room_phase.save
        
        val talk = Talk.create.roomround_id(game_hall.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                       .message("村莊建立 " + (new java.util.Date).toString)
        talk.save
      }

      /*
      try {
        val plurk_client = new PlurkClient(plurk_apiKey)     // 建立 SPlurk 物件
        plurk_client.Users.login (plurk_username, plurk_password)  // 登入噗浪

        // 發噗
        plurk_client.Timeline.plurkAdd (
          qualifier = Qualifier.Says,  // 設定噗文前的修飾詞（說、喜歡、正在……等）
          content   = "第" + room.id.is.toString + "號村已建立",  // 噗文的內容
          language  = Some(Language.tr_ch)  // 修飾詞的語言（tr_ch 為中文）
        )
      } catch { case e: Exception =>  S.notice("Plurk 發佈失敗") }
      */

      
      S.notice(room.id.toString() + "號村已建立") 
    }
    
    "name=room_name"      #> SHtml.text(room_name, x => room_name = x) & 
    "name=room_comment"   #> SHtml.text(room_comment, x => room_comment = x) & 
    "name=max_user"       #> SHtml.select(Seq(("4","4"),("5","5"),("6","6"),("7","7"),("8","8"),("9","9"),("10","10")),
                             Full(max_user.toString),  x => asInt(x).foreach(y => (max_user = y))) &
    "name=move_time"      #> SHtml.text(move_time.toString,     s => asInt(s).foreach(x => move_time = x)) &
    "name=action_time"    #> SHtml.text(action_time.toString,   s => asInt(s).foreach(x => action_time = x)) &
    "name=reaction_time"  #> SHtml.text(reaction_time.toString, s => asInt(s).foreach(x => reaction_time = x)) &
    "name=test_mode"      #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.TEST_MODE)) &
    "name=wish_align"     #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.WISH_ALIGN)) &
    "name=wish_role"      #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.WISH_ROLE)) &
    //"name=death_look"     #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.DEATH_LOOK)) &
    "name=expansion_role" #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.EXPANSION_ROLE)) &
    "name=custom_role"    #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.CUSTOM_ROLE)) &
    "name=init_location"  #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.INIT_LOCATION)) &
    "name=init_green"     #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.INIT_GREEN)) &
    "name=random_position" #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.RANDOM_POSITION)) &
    "name=four_neutral"   #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.FOUR_NEUTRAL)) &
    "name=ultrasoul_ray"  #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.ULTRASOUL_RAY)) &
    "name=unseen_resist"  #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.UNSEEN_RESIST)) &
    "name=vampire_weaken" #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.VAMPIRE_WEAKEN)) &
    "name=valkyrie_enhance" #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.VALKYRIE_ENHANCE)) &
    "name=vghost_expand"  #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.VGHOST_EXPAND)) &
    "name=emi_enhance"    #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.EMI_ENHANCE)) &
    "name=ellen_heal"     #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.ELLEN_HEAL)) &
    "name=evan_heal"      #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.EVAN_HEAL)) &
    "name=franklin_reuse" #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.FRANKLIN_REUSE)) &
    "name=george_reuse"   #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.GEORGE_REUSE)) &
    "name=angel_choose"   #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.ANGEL_CHOOSE)) &
    "name=adecoy_intimate" #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.ADECOY_INTIMATE)) &
    "name=bellandona_choose" #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.BELLANDONA_CHOOSE)) &
    "name=blackcard_dagger" #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.BLACKCARD_DAGGER)) &
    "name=whitecard_tea"    #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.WHITECARD_TEA)) &
    "name=greencard_hunterheal2"  #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.GREENCARD_HUNTERHEAL2)) &
    "name=greencard_lifeunder11_2" #> SHtml.checkbox(false, if (_) option_list = option_list ::: List(RoomFlagEnum.GREENCARD_LIFEUNDER11_2)) &
    //"type=submit"         #> SHtml.onSubmitUnit(() => debug("TEST")) 
     "type=submit"         #> SHtml.onSubmitUnit(S.callOnce(process))
  }
}
