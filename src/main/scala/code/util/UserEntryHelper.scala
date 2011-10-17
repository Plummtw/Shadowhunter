package org.plummtw.shadowhunter.util

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

import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.card._


object UserEntryHelper {
  // http://74.82.5.143/
  // http://identicon.relucks.org/
  def user_cell(room : Room, roomphase: RoomPhase, currentuserentry: UserEntry, userentry: UserEntry, reveal: Boolean ) : NodeSeq = {
    //val room = Room_R.get
    //val roomround = RoomRound_R.get
    //val roomphase = RoomPhase_R.get
    //val currentuserentry = CurrentUserEntry_R.get
    
    var result : NodeSeq = Seq()
    val user_icon : UserIcon = userentry.get_user_icon
    val id_icon : NodeSeq =
      if ((room.status.is != RoomStatusEnum.PLAYING.toString) || (userentry.ip_address.is != userentry.ip_address0.is))
        Seq(<img src={AdminHelper.identicon_link + userentry.ip_address_md5.is} />)
      else
        NodeSeq.Empty

    if (userentry.live.is)
      result ++= <td valign="top" class={if (userentry.has_room_flag(UserEntryRoomFlagEnum.VOTED)) "voted" else ""}>
      <img src={user_icon.icon_filename.is} width={user_icon.icon_width.is.toString} height={user_icon.icon_height.is.toString} border="2" style={"border-color:" + user_icon.color.is} /><br/><span>
      {"損傷:" + userentry.damaged.is}</span></td>
    else
      result ++= <td valign="top" class="dead"><img src="images/grave.gif"   border="2"  onMouseover={"this.src='" + user_icon.icon_filename.is + "'"} onMouseout="this.src='images/grave.gif'"  style={"border-color:" + user_icon.color.is} /><br/><span>
      {"損傷:" + userentry.damaged.is}</span></td>

    // <img src={"http://identicon.relucks.org/" + user_entry.ip_address_md5.is} />
    val result2 = <td class={if (!userentry.live.is) "dead" 
                             else if (userentry.has_room_flag(UserEntryRoomFlagEnum.VOTED)) "voted" 
                             else if (userentry.id.is == roomphase.player.is) "player-mark" 
                             else ""}>
          <font color={user_icon.color.is}>◆</font>{if (currentuserentry.target_user.is == userentry.id.is) <span class="neutral">{userentry.handle_name.is}</span> else <span>{userentry.handle_name.is}</span>}<br/>
          { if (userentry.trip.is != "")
              Seq(<span>◆</span>, <a target="_blank" class="decor_none" href={AdminHelper.trip_link + userentry.trip.is}>{userentry.trip.is}</a>, <br/>)
            else
              NodeSeq.Empty }
          { if ((reveal || (currentuserentry == userentry) || (userentry.revealed)) && (roomphase.phase_type != RoomPhaseEnum.GAMEHALL.toString))
             <strong>{userentry.get_role_field}</strong>
            else <strong>[？？]</strong>}
          {if (userentry.revealed) "[開]" else "[蓋]"}<br/>
          {id_icon}({ if (userentry.live.is) "生存中" else "死亡" }){
             if (userentry.has_user_flag(UserEntryFlagEnum.LOVER)) <span class="lover">戀</span> else NodeSeq.Empty}{
             if (userentry.has_user_flag(UserEntryFlagEnum.SEALED)) <span class="hunter">封</span> else NodeSeq.Empty}{
             if (userentry.has_user_flag(UserEntryFlagEnum.POISON)) {
               val num = userentry.user_flags.is.count(_.toString == UserEntryFlagEnum.POISON.toString)
               if (num == 1)
                 <span class="shadow">毒</span> 
               else
                 <span class="shadow">毒{num.toString}</span> 
             } else NodeSeq.Empty
             }{
             if (userentry.has_user_flag(UserEntryFlagEnum.FROG)) <span class="shadow">蛙</span> else NodeSeq.Empty}{
             if (userentry.has_user_flag(UserEntryFlagEnum.TAUNT)) <span class="neutral">諷</span> else NodeSeq.Empty}{
             if (userentry.has_user_flag(UserEntryFlagEnum.GUARDIAN)) <span class="guardian">守</span> else NodeSeq.Empty}{
             if (userentry.has_user_flag(UserEntryFlagEnum.BARRIER)) <span class="hunter">防</span> else NodeSeq.Empty}<br/>
          {userentry.items.map(x=> x.asInstanceOf[Equipment].equip_name).mkString("")}
         </td>
    
    result ++ result2
  }


  def user_admin_cell(room: Room, roomphase: RoomPhase, userentry: UserEntry ) : NodeSeq = {
    var result : NodeSeq = Seq()
    val user_icon : UserIcon = userentry.get_user_icon
    val id_icon : NodeSeq =
      if ((room.status.is != RoomStatusEnum.PLAYING.toString) || (userentry.ip_address.is != userentry.ip_address0.is))
        Seq(<img src={AdminHelper.identicon_link + userentry.ip_address_md5.is} />)
      else
        NodeSeq.Empty

    if (userentry.live.is)
      result ++= <td valign="top" class={if (userentry.has_room_flag(UserEntryRoomFlagEnum.VOTED)) "voted" else ""}>
      <img src={user_icon.icon_filename.is} width={user_icon.icon_width.is.toString} height={user_icon.icon_height.is.toString} border="2" style={"border-color:" + user_icon.color.is} /><br/><span>
      {"損傷:" + userentry.damaged.is}</span></td>
    else
      result ++= <td valign="top" class="dead"><img src="images/grave.gif"   border="2"  onMouseover={"this.src='" + user_icon.icon_filename.is + "'"} onMouseout="this.src='images/grave.gif'"  style={"border-color:" + user_icon.color.is} /><br/><span>
      {"損傷:" + userentry.damaged.is}</span></td>

    // <img src={"http://identicon.relucks.org/" + user_entry.ip_address_md5.is} />
    val result2 = <td class={if (!userentry.live.is) "dead" 
                             else if (userentry.has_room_flag(UserEntryRoomFlagEnum.VOTED)) "voted" 
                             else if (userentry.id.is == roomphase.player.is) "player-mark" 
                             else ""}>
          <font color={user_icon.color.is}>◆</font>{userentry.handle_name.is}<br/>
          { if (userentry.trip.is != "")
              Seq(<span>◆</span>, <a target="_blank" class="decor_none" href={AdminHelper.trip_link + userentry.trip.is}>{userentry.trip.is}</a>, <br/>)
            else
              NodeSeq.Empty }
           <strong>{userentry.get_role_field}</strong>
          {if (userentry.revealed) "[開]" else "[蓋]"}<br/>
          {Seq(<input type="checkbox" id={"id" + userentry.user_no.is} name={"id" + userentry.user_no.is} />)}
          {id_icon}({ if (userentry.live.is) "生存中" else "死亡" }){
             if (userentry.has_user_flag(UserEntryFlagEnum.LOVER)) <span class="lover">戀</span> else NodeSeq.Empty}{
             if (userentry.has_user_flag(UserEntryFlagEnum.SEALED)) <span class="hunter">封</span> else NodeSeq.Empty}{
             if (userentry.has_user_flag(UserEntryFlagEnum.POISON)) {
               val num = userentry.user_flags.is.count(_.toString == UserEntryFlagEnum.POISON.toString)
               if (num == 1)
                 <span class="shadow">毒</span> 
               else
                 <span class="shadow">毒{num.toString}</span> 
             } else NodeSeq.Empty}{
             if (userentry.has_user_flag(UserEntryFlagEnum.FROG)) <span class="shadow">蛙</span> else NodeSeq.Empty}{
             if (userentry.has_user_flag(UserEntryFlagEnum.TAUNT)) <span class="neutral">諷</span> else NodeSeq.Empty}{
             if (userentry.has_user_flag(UserEntryFlagEnum.GUARDIAN)) <span class="guardian">守</span> else NodeSeq.Empty}{
             if (userentry.has_user_flag(UserEntryFlagEnum.BARRIER)) <span class="hunter">防</span> else NodeSeq.Empty}<br/>
          {userentry.items.map(x=> x.asInstanceOf[Equipment].equip_name).mkString("")}
         </td>
    
    result ++ result2
    /*    
    var result : NodeSeq = Seq()
    val user_icon : UserIcon = user_entry.get_user_icon()
    val id_icon : NodeSeq =
      if ((room.status.is != RoomStatusEnum.PLAYING.toString) || (user_entry.ip_address.is != user_entry.ip_address0.is))
        Seq(<img src={LinkHelper.identicon + user_entry.ip_address_md5.is} />)
      else
        NodeSeq.Empty

    if (user_entry.live.is)
      result ++= <td valign="top" bgcolor={if (user_entry.user_flags.is == UserEntryFlagEnum.VOTED.toString) "#FF50FF" else ""}>
      <img src={user_icon.icon_filename.is} width={user_icon.icon_width.is.toString} height={user_icon.icon_height.is.toString} border="2" style={"border-color:" + user_icon.color.is} /></td>
    else
      result ++= <td valign="top" bgcolor="#992222"><img src="images/grave.gif"   border="2"  onMouseover={"this.src='" + user_icon.icon_filename.is + "'"} onMouseout="this.src='images/grave.gif'"  style={"border-color:" + user_icon.color.is} /></td>

    // <img src={"http://identicon.relucks.org/" + user_entry.ip_address_md5.is} />
    result ++= <td bgcolor={if (!user_entry.live.is) "#992222" else if (user_entry.user_flags.is == UserEntryFlagEnum.VOTED.toString) "#FF50FF" else ""}>
          <font color={user_icon.color.is}>◆</font>{user_entry.handle_name.is}<br/>
          {
            if (user_entry.trip.is != "")
              Seq(<span>◆</span>, <a target="_blank" style="text-decoration: none;" href={LinkHelper.trip + user_entry.trip.is}>{user_entry.trip.is}</a>, <br/>)
            else
              NodeSeq.Empty
          }
          {
             Seq(<strong>{user_entry.get_role_field()}<br/>{user_entry.get_subrole_field()}</strong>)
          }

          {Seq(<input type="checkbox" id={"id" + user_entry.user_no.is} name={"id" + user_entry.user_no.is} />)}
          {id_icon}({ if (user_entry.live.is) "生存中" else "死亡" })
          {if (user_entry.has_flag(UserEntryFlagEnum.RELIGION) ) Seq(<font color="#EEAA55">教</font>) else NodeSeq.Empty}
         </td>

    return result
    */
  }

  /*
  def user_select_cell(user_entry : UserEntry, targetable : Boolean ) : NodeSeq = {
    var result : NodeSeq = Seq()
    val user_icon : UserIcon = user_entry.get_user_icon()
    if (user_entry.live.is)
      result ++= <td class="table_votelist1" valign="top" bgcolor={if (user_entry.user_flags.is == UserEntryFlagEnum.VOTED.toString) "#FF50FF" else ""}>
      <img src={user_icon.icon_filename.is} width={user_icon.icon_width.is.toString} height={user_icon.icon_height.is.toString} border="2" style={"border-color:" + user_icon.color.is} /></td>
    else
      result ++= <td class="table_votelist1" valign="top" bgcolor="#992222"><img src="images/grave.gif"   border="2"  onMouseover={"this.src='" + user_icon.icon_filename.is + "'"} onMouseout="this.src='images/grave.gif'"  style={"border-color:" + user_icon.color.is} /></td>

    result ++= <td class="table_votelist2" width="150px" bgcolor={if (!user_entry.live.is) "#992222" else if (user_entry.user_flags.is == UserEntryFlagEnum.VOTED.toString) "#FF50FF" else ""}>
          {user_entry.handle_name.is}<br/><font color={user_icon.color.is}>◆</font>
          { if (targetable)
             Seq(<input type="radio" id="target" name="target" value={user_entry.id.is.toString} />)
            else
             NodeSeq.Empty}
         </td>
   return result
  }
  */

  def user_select_cell(userentry : UserEntry, node : NodeSeq ) : NodeSeq = {
    var result : NodeSeq = Seq()
    val user_icon : UserIcon = userentry.get_user_icon
    
    if (userentry.live.is)
      result ++= <td valign="top" class={if (userentry.has_room_flag(UserEntryRoomFlagEnum.VOTED)) "voted" else ""}>
      <img src={user_icon.icon_filename.is} width={user_icon.icon_width.is.toString} height={user_icon.icon_height.is.toString}
           border="2" style={"border-color:" + user_icon.color.is} /></td>
    else
      result ++= <td valign="top" class="dead"><img src="images/grave.gif"   border="2"
                     onMouseover={"this.src='" + user_icon.icon_filename.is + "'"} onMouseout="this.src='images/grave.gif'" 
                     style={"border-color:" + user_icon.color.is} /></td>

    result ++= <td class={if (userentry.has_room_flag(UserEntryRoomFlagEnum.VOTED)) "voted" else ""} width="150px" >
      {userentry.handle_name.is}<br/><font color={user_icon.color.is}>◆</font>
      {node}</td>
    result
  }
  
  // User Table
  def user_table(room:Room, roomphase:RoomPhase, currentuserentry:UserEntry, 
                 userentrys:List[UserEntry], reveal: Boolean) : NodeSeq = {
    //val userentrys = UserEntrys_RR.get
    val user_groups = userentrys.grouped(5).toList
    //room:Room, current_user:UserEntry, user_entrys: List[UserEntry], 

    <table border="0" cellpadding="0" cellspacing="5" class="userentry_table"> <tbody>   
    { for (user_group <- user_groups) yield <tr> { 
       for (userentry <- user_group) yield user_cell(room, roomphase, currentuserentry, userentry, reveal)
    } </tr> } </tbody></table> 
  }

  def user_select_table(userentrys: List[UserEntry], targettable_list: List[UserEntry], callback : String => Unit) : NodeSeq = {
    assert(! targettable_list.isEmpty)
        
    //val userentrys = UserEntrys_RR.get
    val user_groups = userentrys.grouped(5).toList
        
    val targettable_id_list = targettable_list.map(_.id.is.toString)
    val targettable_radios = SHtml.radio(targettable_id_list, Full(targettable_id_list(0)), callback(_))
    
        
    <table border="0" cellpadding="0" cellspacing="5" class="userentry_table"><tbody>
    { for (user_group <- user_groups) yield <tr> { 
       for (userentry <- user_group) yield {
         val index = targettable_list.indexOf(userentry)
         if (index != -1)
           user_select_cell(userentry, targettable_radios(index))
         else
           user_select_cell(userentry, NodeSeq.Empty)
       }
    } </tr> } </tbody></table> }
      
  def user_choose_table(userentrys: List[UserEntry], targettable_list: List[UserEntry], callback : Long => Unit) : NodeSeq = {
    assert(! targettable_list.isEmpty)
        
    //val userentrys = UserEntrys_RR.get
    val user_groups = userentrys.grouped(5).toList
        
    val targettable_id_list = targettable_list.map(_.id.is.toString)
    //val targettable_radios = SHtml.radio(targettable_id_list, Full(targettable_id_list(0)), callback(_))
    
        
    <table border="0" cellpadding="0" cellspacing="5" class="userentry_table"><tbody>
    { for (user_group <- user_groups) yield <tr> { 
       for (userentry <- user_group) yield {
         val index = targettable_list.indexOf(userentry)
         if (index != -1) {
           val targettable_checkbox = SHtml.checkbox(false, if (_) callback(userentry.id.is)) 
           user_select_cell(userentry, targettable_checkbox)
         } else
           user_select_cell(userentry, NodeSeq.Empty)
       }
    } </tr> } </tbody></table> }
          
          
  // User Admin Table
  def user_admin_table(room:Room, roomphase:RoomPhase, userentrys: List[UserEntry]) : NodeSeq = {
    val user_groups = userentrys.grouped(5).toList

    return <table border="0" cellpadding="0" cellspacing="5" style="userentry_table">
    { for (user_group <- user_groups) yield <tr> {
       for (userentry <- user_group) yield user_admin_cell(room, roomphase, userentry)
    } </tr> } </table> }

  /*
  // User Select Table
  def user_select_table(user_entrys : List[UserEntry], targettable : List[UserEntry]) : NodeSeq = {
    val user_groups = JinrouUtil.zipListBySize(5)(user_entrys)

    return <table border="0"><tr>
             <td width="1000">
              <table border="0">
    { for (val user_group <- user_groups) yield <tr> { 
       for (val user_entry <- user_group) yield user_select_cell(user_entry, targettable.contains(user_entry))
    } </tr> } </table></td></tr></table> }
   */
}


