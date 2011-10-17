package org.plummtw.shadowhunter.model 
 
import net.liftweb._ 
import mapper._ 
import http._ 
import SHtml._ 
import util._

class Action extends LongKeyedMapper[Action]  with CreatedUpdated with IdPK {
  def getSingleton = Action // what's the "meta" object
  //def primaryKeyField = id

  // the primary key
  // object id            extends MappedLongIndex(this)
  object roomround_id    extends MappedLongForeignKey(this, RoomRound) 
  object actioner_id       extends MappedLongForeignKey(this, UserEntry)
  object actionee_id      extends MappedLongForeignKey(this, UserEntry)
  //object cssclass        extends MappedString(this, 20)
  
  object content        extends MappedString(this, 20)
  object mtype          extends MappedString(this, 4)
  
  object action_flags  extends MappedString(this, 20)
}

object Action extends Action with LongKeyedMetaMapper[Action] {
  override def fieldOrder = List(id, roomround_id, actioner_id, content, 
                              mtype, action_flags)
}