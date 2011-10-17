package org.plummtw.shadowhunter.enum

object RoomPhaseEnum extends Enumeration {
  type RoomPhaseEnum = Value
  
  val NONE            = Value("")
  
  val GAMEHALL        = Value("GH")
  val MOVEMENT        = Value("MV")
  //val MOVEMENT_SKILL  = Value("MS")
  val LOCATION        = Value("LO")
  val LOCATION_CHOOSE = Value("LC")
  val CARD            = Value("CA")
  val CARD_CHOOSE     = Value("CC")
  val CARD_SKILL      = Value("CS")
  val GREEN_REACTION  = Value("GR")
  val ATTACK          = Value("AT")
  val REACTION        = Value("RE")
  val POST_ATTACK     = Value("PA")
  val ENDED           = Value("EN")
  
  val CNAME_MAP     = scala.collection.immutable.TreeMap(
    NONE              -> "無",
    GAMEHALL         -> "大廳行動",
    MOVEMENT         -> "移動",
    LOCATION          -> "地形行動",
    CARD              -> "卡片行動",
    CARD_SKILL        -> "卡片行動",
    CARD_CHOOSE       -> "卡片行動",
    ATTACK            -> "攻擊",
    POST_ATTACK       -> "後置行動",
    ENDED             -> "結束行動"
  )
  
  def get_cname(phase : RoomPhaseEnum.Value) : String =
    CNAME_MAP.get(phase).getOrElse("無")
  
  def get_cname(phase : String ) : String =
    get_cname(try {RoomPhaseEnum.withName(phase)}
      catch {case e : Exception => NONE})
  
  implicit def gamephaseenum2String (en : RoomPhaseEnum.Value) : String = en.toString
}
