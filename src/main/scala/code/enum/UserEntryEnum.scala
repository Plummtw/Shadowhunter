package org.plummtw.shadowhunter.enum

object UserEntryRoomFlagEnum extends Enumeration {
  type UserEntryRoomFlagEnum = Value
  
  val  VOTED     = Value("V")
  val  AUTOVOTED = Value("A")
  val  SKIPPED    = Value("S")
  val  SUDDENDEATH = Value("D")
}

object UserEntryRoleFlagEnum extends Enumeration {
  type UserEntryRoleFlagEnum = Value
  
  val  AMBUSH              = Value("A")
  val  ROLE_SKILL_USED     = Value("U")
  val  ROLE_MOVESKILL_USED = Value("M")
  val  REVIVED_AURA        = Value("R")
  val  ENHANCED            = Value("E")
}

object UserEntryFlagEnum extends Enumeration {
  type UserEntryFlagEnum = Value
  
  val  SEALED  = Value("S")
  val  VICTORY = Value("V")
  val  VICTORY2 = Value("W")
  
  val  ADVENT  = Value("A")
  val  CHOCOLATE = Value("C")
  val  DIABOLIC  = Value("D")
  
  val  GUARDIAN  = Value("G")
  val  BARRIER    = Value("B")
  
  val  LOVER     = Value("L")
  val  FROG      = Value("F")
  
  val  POISON    = Value("P")
  val  TAUNT     = Value("T")
  
  val  REASONAED = Value("Z")
  val  REVIVED   = Value("R")
  
  val  MISSED    = Value("M")
}


