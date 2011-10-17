package org.plummtw.shadowhunter.enum

import org.plummtw.shadowhunter.data._

object MTypeEnum extends Enumeration {
  type MTypeEnum = Value
  
  val NONE               = Value("")
  
  val TALK                = Value("T")
  val TALK_DAY            = Value("TD")
  val TALK_ADMIN         = Value("TA")
  val TALK_ADMIN_PRIVATE = Value("TAP")
  val TALK_HEAVEN        = Value("TH")
  val TALK_END            = Value("TE")
  
  val MESSAGE               = Value("S")
  val MESSAGE_HIDDEN       = Value("SH")
  val MESSAGE_GENERAL      = Value("S0")
  val MESSAGE_COME         = Value("S1")
  val MESSAGE_LEAVE        = Value("S2")
  val MESSAGE_KICKED       = Value("S3")
  val MESSAGE_REVOTE0      = Value("S4")
  val MESSAGE_LAST2MIN     = Value("S5")
  val MESSAGE_DEATHSUDDEN = Value("S6")
  val MESSAGE_REVOTE       = Value("S7")
  val MESSAGE_TIMEOUT      = Value("S8")
  val MESSAGE_DEATH        = Value("S9")
  
  val RESULT_MOVE          = Value("RM")
  val RESULT_ATTACK        = Value("RA")
  val RESULT_GREENCARD    = Value("RG")
  val RESULT_GREENREVEAL   = Value("RV")
  val RESULT_WHITECARD    = Value("RW")
  val RESULT_BLACKCARD    = Value("RB")

  val OBJECTION_MALE    = Value("OM")
  val OBJECTION_FEMALE  = Value("OF")
  
  val ACTION_NO_ACTION   = Value("AN")
  val ACTION_TEST_ALERT  = Value("A0")
  val ACTION_KICK        = Value("AK")
  val ACTION_STARTGAME  = Value("AS")
  val ACTION_FLIP         = Value("AF")
  val ACTION_MOVE        = Value("AM")
  val ACTION_ATTACK      = Value("AA")
  val ACTION_MULTIATTACK = Value("AU")
  val ACTION_NOATTACK    = Value("AB")
  val ACTION_NEXTROUND   = Value("AR")
  val ACTION_CARDCHOOSE  = Value("AC")
  val ACTION_ITEMPREFERRED = Value("AI")
  
  val ACTION_DRAWBLACKCARD = Value("DB")
  val ACTION_DRAWWHITECARD = Value("DW")
  val ACTION_DRAWGREENCARD = Value("DG")
  val ACTION_NOLOC           = Value("DN")
  val ACTION_LOCDAMAGE      = Value("DD")
  val ACTION_LOCHEAL         = Value("DH")
  val ACTION_LOCROB          = Value("DR")

  val ACTION_WHITECARD        = Value("WC")
  val ACTION_BLACKCARD        = Value("BC")
  val ACTION_GREENCARD        = Value("GC")
  
  val ACTION_ULTRASOUL_URAY    = Value("AUS1")
  val ACTION_ULTRASOUL_RAY     = Value("AUS2")
  val ACTION_UNKNOWN_DECEIVE   = Value("AUN1")
  val ACTION_VENGEFULGHOST_CURSE = Value("AVG1")
  val ACTION_WEREWOLF_COUNTER  = Value("AWW1")
  val ACTION_WEREWOLF_AMBUSH   = Value("AWW2")
  val ACTION_WIGHT_MANIPULATE   = Value("AWI1")
  val ACTION_WITCH_TOAD          = Value("AWT1")
  val ACTION_EVAN_BRACEUP        = Value("AEV1")
  val ACTION_ELLEN_CURSECHAIN    = Value("AEL1")
  val ACTION_EMI_TELEPORT         = Value("AEM1")
  val ACTION_FRANKLIN_LIGHTNING  = Value("AFR1")
  val ACTION_FUKA_DYNAMITEHEAL  = Value("AFK1")
  val ACTION_FATHEROCONNEL_PRAY = Value("AFO1")
  val ACTION_FENG_KIKOU          = Value("AFE1")
  val ACTION_GEORGE_DEMOLISH     = Value("AGE1")
  val ACTION_GREGOR_BARRIER      = Value("AGR1")
  val ACTION_GINGER_TOUGHNESS    = Value("AGI1")
  val ACTION_GODFAT_EXCHANGE    = Value("AGF1")
  val ACTION_ALLIE_MOTHERLOVE    = Value("AAL1")
  val ACTION_ANGEL_REINCARNATE   = Value("AAN1")
  val ACTION_ADECOY_TAUNT        = Value("AAD1")
  val ACTION_BOMB_BOMB           = Value("ABM1")
  val ACTION_CHARLES_BLOODFEAST  = Value("ACH1")
  val ACTION_CASSANDRA_FATECHANGE = Value("ACS1")
  val ACTION_CASSANDRA_GIVE      = Value("ACS2")
  val ACTION_DAVID_GRAVEDIG      = Value("ADV1")
  
  val ACTION_DETECTIVE_REASONA   = Value("ADT1")
  val ACTION_DETECTIVE_REASONR   = Value("ADT2")
  
  def get_action(string : String) : MTypeEnum.Value = {
    try {MTypeEnum.withName(string)}
    catch {case e: Exception => NONE}
  }
  
  implicit def mtypeenum2String (en : MTypeEnum.Value) : String = en.toString
}