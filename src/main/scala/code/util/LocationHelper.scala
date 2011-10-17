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
// import scala.xml.Unparsed

import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.util._

object LocationHelper {
  def neighbor(room : Room, loc : String) : String = {
    //val room = Room_R.get
    val location_str = room.room_arrange.is
    
    val location_index = location_str.indexOf(loc)
    
    if (location_index % 2 == 0)
      location_str.substring(location_index+1, location_index+2)
    else
      location_str.substring(location_index-1, location_index)
  }
  
  def left(room : Room, loc : String) : String = {
    //val room = Room_R.get
    val location_str = room.room_arrange.is
    
    val location_index = location_str.indexOf(loc)
    
    val new_index = (location_index + location_str.length - 1) % location_str.length
    
    location_str.substring(new_index, new_index + 1)
  }
  
  def right(room : Room, loc : String) : String = {
    //val room = Room_R.get
    val location_str = room.room_arrange.is
    
    val location_index = location_str.indexOf(loc)
    
    val new_index = (location_index + 1) % location_str.length
    
    location_str.substring(new_index, new_index + 1)
  }
  
  def location_table(room: Room, userentrys: List[UserEntry]) = {
    //val room = Room_R.get
    val location_str = room.room_arrange.is
    
    if ((location_str == null) || (location_str.length < 6))
      NodeSeq.Empty
    else {
      //val userentrys = UserEntrys_RR.get
    
      val locations = location_str.toList.map(_.toString).grouped(2)
    
      Seq (<table border="1"> {
        for (location_group <- locations) yield
        <tr> { 
          for (location <- location_group) yield
          Seq(<td>{LocationEnum.get_cname(location)}</td>,
              <td>{userentrys.filter(_.location.is == location).map(_.handle_name.is).mkString("　")}</td>)
        } </tr>
      } </table> )
    }
  }  

      
}