package org.plummtw.shadowhunter.enum

import org.plummtw.shadowhunter.card._

object CardTypeEnum extends Enumeration {
  type CardTypeEnum = Value
  
  val NONE  = Value("")
  val WHITE = Value("W")
  val BLACK = Value("B")
  val GREEN = Value("G")
  
  implicit def cardtypeenum2String (en : CardTypeEnum.Value) : String = en.toString
}

object CardEnum extends Enumeration {
  type CardEnum = Value
  
  val NONE                       = Value("")
  
  val PREFER_ITEM                = Value("000")
  val PREFER_LIFE                = Value("111")
  
  val W_HOLY_WATER_OF_HEALING = Value("W01")  // *2
  val W_TALISMAN                = Value("W02")
  val W_FORTUNE_BROOCH         = Value("W03")
  val W_MYSTIC_COMPASS         = Value("W04")
  val W_HOLY_ROBE              = Value("W05")
  val W_SILVER_ROSARY          = Value("W06")
  val W_LANCE_OF_LONGINUS     = Value("W07")
  val W_ADVENT                 = Value("W08")
  val W_CHOCOLATE              = Value("W09")
  val W_BLESSING                = Value("W10")
  val W_CONCEALED_KNOWLEDGE  = Value("W11")
  val W_GUARDIAN_ANGEL        = Value("W12")
  val W_FLARE_OF_JUDGEMENT    = Value("W13")
  val W_DISENCHANTED_MIRROR   = Value("W14")
  val W_FIRST_AID               = Value("W15")
  val W_TEA                     = Value("W16")

  val B_CHAINSAW               = Value("B01")
  val B_BUTCHER_KNIFE           = Value("B02")
  val B_RUSTED_BROAD_AXE       = Value("B03")
  val B_VAMPIRE_BAT             = Value("B04")  // *3
  val B_BLOODTHIRSTY_SPIDER     = Value("B05")
  val B_MOODY_GOBLIN           = Value("B06") // *2
  val B_MASAMUNE               = Value("B07")
  val B_MACHINE_GUN            = Value("B08")
  val B_HANDGUN                = Value("B09")
  val B_SPIRITUAL_DOLL           = Value("B10")
  val B_DYNAMITE                = Value("B11")
  val B_DIABOLIC_RITUAL          = Value("B12")
  val B_BANANA_PEEL             = Value("B13")
  val B_DAGGER                  = Value("B14")

  val G_SHADOW_LOSE1           = Value("G01")  
  val G_SHADOW_LOSE2           = Value("G02")
  val G_HUNTER_LOSE1            = Value("G03") //*2
  val G_SHADOW_HUNTER_EQUIP   = Value("G04") //*2
  val G_SHADOW_NEUTRAL_EQUIP  = Value("G05") //*2
  val G_HUNTER_NEUTRAL_EQUIP   = Value("G06") //*2
  val G_SHADOW_HEAL1           = Value("G07")
  val G_HUNTER_HEAL1            = Value("G08")
  val G_NEUTRAL_HEAL1           = Value("G09")
  val G_LIFE_OVER12              = Value("G10")
  val G_LIFE_UNDER11             = Value("G11")
  val G_REVEAL_PREV             = Value("G12")
  val G_HUNTER_HEAL2            = Value("G13")
  val G_LIFE_UNDER11_2            = Value("G14")
  
  val EQUIPMENT_LIST = List(W_TALISMAN, W_FORTUNE_BROOCH, W_MYSTIC_COMPASS,
    W_HOLY_ROBE, W_SILVER_ROSARY, W_LANCE_OF_LONGINUS,
    B_CHAINSAW, B_BUTCHER_KNIFE, B_RUSTED_BROAD_AXE,
    B_MASAMUNE, B_MACHINE_GUN, B_HANDGUN, B_DAGGER)
  
  val WHITE_LIST = List(W_HOLY_WATER_OF_HEALING, W_HOLY_WATER_OF_HEALING,
    W_TALISMAN, W_FORTUNE_BROOCH, W_MYSTIC_COMPASS, W_HOLY_ROBE,
    W_SILVER_ROSARY, W_LANCE_OF_LONGINUS, W_ADVENT, W_CHOCOLATE,
    W_BLESSING, W_CONCEALED_KNOWLEDGE, W_GUARDIAN_ANGEL, W_FLARE_OF_JUDGEMENT,
    W_DISENCHANTED_MIRROR, W_FIRST_AID)

  val BLACK_LIST = List(B_CHAINSAW, B_BUTCHER_KNIFE, B_RUSTED_BROAD_AXE,
    B_VAMPIRE_BAT, B_VAMPIRE_BAT, B_VAMPIRE_BAT, B_BLOODTHIRSTY_SPIDER,
    B_MOODY_GOBLIN, B_MOODY_GOBLIN, B_MASAMUNE,
    B_MACHINE_GUN, B_HANDGUN, B_SPIRITUAL_DOLL, B_DYNAMITE,
    B_DIABOLIC_RITUAL, B_BANANA_PEEL)

  val GREEN_LIST = List(G_SHADOW_LOSE1, G_SHADOW_LOSE2, G_HUNTER_LOSE1, G_HUNTER_LOSE1,
    G_SHADOW_HUNTER_EQUIP, G_SHADOW_HUNTER_EQUIP,
    G_SHADOW_NEUTRAL_EQUIP, G_SHADOW_NEUTRAL_EQUIP,
    G_HUNTER_NEUTRAL_EQUIP, G_HUNTER_NEUTRAL_EQUIP,
    G_SHADOW_HEAL1, G_HUNTER_HEAL1, G_NEUTRAL_HEAL1,
    G_LIFE_OVER12, G_LIFE_UNDER11, G_REVEAL_PREV)
  
  val CARD_MAP   = scala.collection.immutable.TreeMap(
    PREFER_ITEM               -> PreferItem,
    PREFER_LIFE               -> PreferLife,
    
    W_HOLY_WATER_OF_HEALING  -> WCardHolyWaterOfHealing,
    W_TALISMAN               -> WCardTalisman,
    W_FORTUNE_BROOCH         -> WCardFortuneBrooch,
    W_MYSTIC_COMPASS         -> WCardMysticCompass,
    W_HOLY_ROBE              -> WCardHolyRobe,
    W_SILVER_ROSARY          -> WCardSilverRosary,
    W_LANCE_OF_LONGINUS     -> WCardLanceOfLonginus,
    W_ADVENT                 -> WCardAdvent,
    W_CHOCOLATE              -> WCardChocolate,
    W_BLESSING               -> WCardBlessing,
    W_CONCEALED_KNOWLEDGE  -> WCardConcealedKnowledge,
    W_GUARDIAN_ANGEL        -> WCardGuardianAngel,
    W_FLARE_OF_JUDGEMENT    -> WCardFlareOfJudgement,
    W_DISENCHANTED_MIRROR   -> WCardDisenchantedMirror,
    W_FIRST_AID               -> WCardFirstAid,
    W_TEA                     -> WCardTea,
    
    B_CHAINSAW               -> BCardChainsaw,
    B_BUTCHER_KNIFE           -> BCardButcherKnife,
    B_RUSTED_BROAD_AXE       -> BCardRustedBroadAxe,
    B_VAMPIRE_BAT            -> BCardVampireBat,
    B_BLOODTHIRSTY_SPIDER     -> BCardBloodthirstySpider,
    B_MOODY_GOBLIN           -> BCardMoodyGoblin,
    B_MASAMUNE               -> BCardMasamune,
    B_MACHINE_GUN            -> BCardMachineGune,
    B_HANDGUN                -> BCardHandGun,
    B_SPIRITUAL_DOLL           -> BCardSpiritualDoll,
    B_DYNAMITE                -> BCardDynamite,
    B_DIABOLIC_RITUAL          -> BCardDiabolicRitual,
    B_BANANA_PEEL             -> BCardBananaPeel,
    B_DAGGER                  -> BCardDagger,
    
    G_SHADOW_LOSE1           -> GShadowLose1,
    G_SHADOW_LOSE2           -> GShadowLose2,
    G_HUNTER_LOSE1           -> GHunterLose1,
    G_SHADOW_HUNTER_EQUIP   -> GShadowHunterEquip,
    G_SHADOW_NEUTRAL_EQUIP -> GShadowNeutralEquip,
    G_HUNTER_NEUTRAL_EQUIP  -> GHunterNeutralEquip,
    G_SHADOW_HEAL1          -> GShadowHeal1,
    G_HUNTER_HEAL1            -> GHunterHeal1,
    G_NEUTRAL_HEAL1          -> GNeutralHeal1,
    G_LIFE_OVER12              -> GLifeOver12,
    G_LIFE_UNDER11            -> GLifeUnder11,
    G_REVEAL_PREV            -> GRevealPrev,
    G_HUNTER_HEAL2           -> GHunterHeal2,
    G_LIFE_UNDER11_2         -> GLifeUnder11_2
  )
  
  def get_card(card : CardEnum.Value) : Card =
    CARD_MAP.get(card).getOrElse(CardNone)
  
  def get_card(card : String) : Card = 
    get_card(
      try { CardEnum.withName(card) }
      catch {case e : Exception => NONE})
  
  implicit def cardenum2String (en : CardEnum.Value) : String = en.toString
}

