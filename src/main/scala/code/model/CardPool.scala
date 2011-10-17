package org.plummtw.shadowhunter.model

import net.liftweb._
import net.liftweb.mapper._
import net.liftweb.util.FieldError

import scala.util.matching.Regex

import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.card._
import org.plummtw.shadowhunter.util.PlummUtil

import net.liftweb.http.SessionVar

object CardPool_R extends SessionVar[List[CardPool]](List())

class CardPool extends LongKeyedMapper[CardPool] with CreatedUpdated with IdPK {
  //override def dbName = "User"
  def getSingleton = CardPool
  
  object room_id       extends MappedLongForeignKey(this, Room)
  //object field_id      extends MappedLongForeignKey(this, Field)
  object card_no        extends MappedInt(this)
  object card_type     extends MappedString(this, 1)
  object card          extends MappedString(this, 4)
  object owner_id      extends MappedLongForeignKey(this, UserEntry)
  
  object discarded     extends MappedBoolean(this)
  object flags         extends MappedString(this, 20)
  
  def to_card : Card = CardEnum.get_card(card.is.toString)
}

object CardPool extends CardPool with LongKeyedMetaMapper[CardPool] {
  override def fieldOrder = List(id, card_no, card_type, card, owner_id, discarded,
                                 flags)
}

