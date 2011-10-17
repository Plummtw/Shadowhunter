package org.plummtw.shadowhunter.util

import scala.xml._
import net.liftweb._
import net.liftweb.mapper._
import http._
import js._
import util._
import S._
import SHtml._
import Helpers._

import collection.mutable.{HashMap, SynchronizedMap}

import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.actor._

/*
class DeadlineHandler(val room_id_in : Long) {
  def room_id : Long = room_id_in

  def check_deadline(room: Room) = {
    //val room = Room.find(By(Room.id, room_id)).get
    val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is),
                                   OrderBy(RoomRound.round_no, Descending)).get
    val roomphase = RoomPhase.find(By(RoomPhase.roomround_id, roomround.id.is),
                                   OrderBy(RoomPhase.phase_no, Descending)).get
    val deadline = roomphase.deadline.is
    val now = new java.util.Date()
    
    if (now.after(deadline)) {
      if (roomphase.phase_type.is == RoomPhaseEnum.GAMEHALL.toString)
        RoomActor ! SignalAbandon(room) 
      else  
        RoomActor ! SignalDeadline(room, roomround, roomphase)
    }
  }  
}

object DeadlineMap {
  val TIMEOUT_CALL_INTERVAL = 5000
  
  val map : HashMap[Long, DeadlineHandler] = new HashMap[Long, DeadlineHandler] with SynchronizedMap[Long, DeadlineHandler]
  
  def create_handler(room_id : Long) = {
    if (!map.contains(room_id)) {
      
    }
      
  }
  
  def check_handler = {
    
  }
  
  var is_thread_stop = false
  def stop {
    is_thread_stop = true
  }
  
  val thread = new Thread {
    override def run() {
      while (!is_thread_stop) {
        Thread.sleep(TIMEOUT_CALL_INTERVAL)
        map.foreach {item =>
          val (room_id, handler) = item
          val room = Room.find(By(Room.id, room_id)).get
          if (room.status.is == RoomStatusEnum.ENDED.toString)
            map.remove(room_id)
          else 
            handler.check_deadline(room)
        }
      }
    }
  }

  thread.start()
}
*/