package org.plummtw.shadowhunter.model

import net.liftweb._
import net.liftweb.mapper._
//import net.liftweb.util.FieldError

import net.liftweb.http.RequestVar
import net.liftweb.http.SessionVar

//import scala.util.matching.Regex

import org.plummtw.shadowhunter.enum._
//import org.plummtw.shadowhunter.util.PlummUtil

object RoomRound_E extends SessionVar[RoomRound](null)
object RoomRound_R extends SessionVar[RoomRound](null)

class RoomRound extends LongKeyedMapper[RoomRound] with CreatedUpdated with IdPK {
  def getSingleton = RoomRound // what's the "meta" object
  //def primaryKeyField = id

  // the primary key
  // object id            extends MappedLongIndex(this)
  object room_id       extends MappedLongForeignKey(this, Room)
  object last_round    extends MappedLongForeignKey(this, RoomRound) 
  
  object round_no      extends MappedInt(this)
  //object player_no      extends MappedInt(this)
  //object round_step     extends MappedString(this,2)
  
  //object reaction_player  extends MappedLongForeignKey(this, UserEntry) 
  //object reaction_type    extends MappedString(this,2)
  
  //object deadline       extends MappedDateTime(this)
  
}

object RoomRound extends RoomRound with LongKeyedMetaMapper[RoomRound] {
  override def fieldOrder = List(id, room_id, last_round, round_no) //, player_no, round_step, reaction_player, reaction_type, deadline
}

