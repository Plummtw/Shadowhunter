package org.plummtw.shadowhunter.enum

object LocationEnum extends Enumeration {
  type LocationEnum = Value
  //* Church (white)
//* Graveyard (Black)
//* Hermit's Cabin (Green)
//* Weird Woods (Grey)
//* Underworld Gate (Purple)
//* Erstwhile Altar (Brown) 
  
  val NONE  = Value("")
  val HERMIT_CABIN      = Value("H")  // 2 3 
  val UNDERWORLD_GATE = Value("U") // 4 5
  val CHURCH            = Value("C") // 6
  val OPTION            = Value("O") // 7
  val GRAVEYARD       = Value("G") // 8
  val WIERD_WOODS     = Value("W") // 9
  val ERSTWHILE_ALTER   = Value("E") // 10
  
  val REPEAT            = Value("R")
  val TELEPORT          = Value("T")
  val COMPASS           = Value("I")
  
  val DICE_MAP     = scala.collection.immutable.TreeMap(
    2 -> HERMIT_CABIN,
    3 -> HERMIT_CABIN,
    4 -> UNDERWORLD_GATE,
    5 -> UNDERWORLD_GATE,
    6 -> CHURCH,
    7 -> OPTION,
    8 -> GRAVEYARD,
    9 -> WIERD_WOODS,
    10 -> ERSTWHILE_ALTER)
  
  val LOCATION_LIST = List(HERMIT_CABIN, UNDERWORLD_GATE, CHURCH, GRAVEYARD, WIERD_WOODS, ERSTWHILE_ALTER)
  
  val CNAME_MAP     = scala.collection.immutable.TreeMap(
    NONE              -> "無",
    HERMIT_CABIN      -> "隱士木屋(23)",
    UNDERWORLD_GATE -> "時空之門(45)",
    CHURCH           -> "教堂(6)",
    OPTION            -> "自選",
    GRAVEYARD        -> "墳墓(8)",
    WIERD_WOODS      -> "怪異樹林(9)",
    ERSTWHILE_ALTER   -> "供品祭壇(10)",
    REPEAT            -> "自選(相同位置)",
    COMPASS            -> "自選(神秘羅盤)",
    TELEPORT          -> "自選(傳送)"
  )
  
  def get_cname(loc : LocationEnum.Value) : String =
    CNAME_MAP.get(loc).getOrElse("無")
  
  def get_loc(loc : String) : LocationEnum.Value = 
    try { LocationEnum.withName(loc) }
    catch {case e : Exception => NONE}
  
  def get_cname(loc : String) : String = 
    get_cname(get_loc(loc))
  
  def from_dice(dice : Int) : LocationEnum.Value = 
    DICE_MAP.get(dice).getOrElse(NONE)
  
  implicit def locationenum2String (en : LocationEnum.Value) : String = en.toString
}
/*
 * 隱士的小木屋(2、3)：
可選擇抽取一張綠卡(隱士卡)。

教堂(6)：
可選擇抽取一張白卡。

墳墓(8)：
可選擇抽取一張黑卡。

怪異樹林(9)：
可以指定一名玩家，減少其兩滴血量，或是回復其一滴血量。(可指定自己)

供品祭壇(10)：
可以搶奪一張裝備卡。

時空之門(4、5)：
可以從白卡、黑卡、綠卡中選一種，抽取一張。
 */