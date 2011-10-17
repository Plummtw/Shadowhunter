package org.plummtw.shadowhunter.card

import org.plummtw.shadowhunter.enum._

class Card(val enum : CardEnum.Value, val name : String, val type_enum : CardTypeEnum.Value) {
  def card_enum = enum
  def card_name = name  
  def cardtype_enum = type_enum
}

trait Equipment {
  def equip_name = ""
}
//trait SingleUse 
//trait SelectOnePlayer

object CardNone extends Card(CardEnum.NONE, "無", CardTypeEnum.NONE)

object PreferItem extends Card(CardEnum.PREFER_ITEM, "亂數裝備", CardTypeEnum.NONE)
object PreferLife extends Card(CardEnum.PREFER_LIFE, "損血", CardTypeEnum.NONE)

/*
 * BLACK CARDSBanana Peel		S	Give one of your equipment cards to another character. 
 * If you have none take 1 point of damage
 * Bloodthirsty Spider	S	Give 2 points of damage to any player's character and take 2 points of damage yourself
 * Butcher Knife		E	If your attack is successful, you give 1 extra point of damage
 * Chainsaw		E	If your attack is successful, you give 1 extra point of damage
 * Diabolic Ritual		S	If you are a Shadow you may reveal your identity and fully heal your damage
 * Dynamite		S	Roll both dice and give 3 points of damage to all characters in that area. On a 7 nothing happensHandgun			E	All ranges but yours become your attack range (you cannot attack in your normal range)Machine Gun		E	Your attack will affect all characters in your attack range. Apply the same damage to all.Masamune		E	You MUST attack and you deal D4 damge. You can't failMoody Goblin		S	Steal an equipment card from any player's characterMoody Goblin		S	Steal an equipment card from any player's characterRusted Broad Axe	E	If your attack is successful, you give 1 extra point of damageSpiritual Doll		S	Pick a player and roll D6 1-4 give their character 3 points of Damage. 5-6 take 3 points of damageVampire Bat		S	Give 2 points of damage to any player's character and heal 1 point of your own.Vampire Bat		S	Give 2 points of damage to any player's character and heal 1 point of your own.Vampire Bat		S	Give 2 points of damage to any player's character and heal 1 point of your own.				WHITE CARDS				Advent			S	If you are a Hunter you may reveal your identity and fully heal your damageBlessing		S	Pick a player character other than yourself and they heal D6 damageChocolate		S	If you're Allie, Emi or Unknown you may reveal your identity and fully heal your damageConcealed Knowledge	S	When your turn is over, it will be your turn againDisenchant Mirror	S	If you are either Vampire or Werewolf you must reveal your identityFirst Aid		S	Set the damage marker of any player's character to 7Flare of Judgement	S	All OTHER characters take 2 points of damageFortune Brooch		E	You take no damage from the weird woods location. You may still heal your own damageGuardian Angel		S	you take no dmage from another characthers attack until your next turnHoly Robe		E	Your attacks do 1 less damage and damage you take from attacks is reduced by 1 pointHoly Water of healing	S	Heal 2 points of your damageHoly Water of healing	S	Heal 2 points of your damageMystic Compas		E	you may roll twice for movement and choose which result to useSilver Rosary		E	If your attack kills another character you get all their equipmentSpear of Longinus	E	If you're a hunter and attack successful you may reveal. If you do or are already revealed you give 2 extra damageTalisman		E	you take no damage from the black cards Bloodthirsty Spider, Vampire Bat or Dynamite		GREEN CARDS				Aid		Hunter heal 1 damage (if you have none take 1 damage)Anger		Hunter or Shadow - give 1 equipment to current player or take 1 damageAnger		Hunter or Shadow - give 1 equipment to current player or take 1 damageBlackmail	Hunter or Neutral - give 1 equipment to current player or take 1 damageBlackmail	Hunter or Neutral - give 1 equipment to current player or take 1 damageBully		HP less or equal 11 (A,B,C,E,U) take 1 damageExorcism	Shadow - take 2 damageGreed		Neutral or Shadow - give 1 equipment to current player or take 1 damageGreed		Neutral or Shadow - give 1 equipment to current player or take 1 damageHuddle		Shadow heal 1 damage (if you have none take 1 damage)Nurturance	Neutral heal 1 damage (if you have none take 1 damage)Predicition	Show your character card to current playerSlap		Hunter - take 1 damageSlap		Hunter - take 1 damageSpell		Shadow - take 1 damageTough Lesson	HP greater or equal 12 (D,F,G,V,W) take 2 damage
 */
