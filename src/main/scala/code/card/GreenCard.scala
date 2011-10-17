package org.plummtw.shadowhunter.card

import org.plummtw.shadowhunter.enum._

class GreenCard(override val enum : CardEnum.Value, override val name : String) extends Card(enum, name, CardTypeEnum.GREEN)

object GShadowLose1 extends GreenCard(CardEnum.G_SHADOW_LOSE1, "暗影損一")
object GShadowLose2 extends GreenCard(CardEnum.G_SHADOW_LOSE2, "暗影損二")
object GHunterLose1 extends GreenCard(CardEnum.G_HUNTER_LOSE1,   "獵人損一")
object GShadowHunterEquip extends GreenCard(CardEnum.G_SHADOW_HUNTER_EQUIP,  "暗影獵人奪取")
object GShadowNeutralEquip extends GreenCard(CardEnum.G_SHADOW_NEUTRAL_EQUIP, "暗影中立奪取")
object GHunterNeutralEquip extends GreenCard(CardEnum.G_HUNTER_NEUTRAL_EQUIP,  "獵人中立奪取")
object GShadowHeal1 extends GreenCard(CardEnum.G_SHADOW_HEAL1, "暗影回一")
object GHunterHeal1 extends GreenCard(CardEnum.G_HUNTER_HEAL1,  "獵人回一")
object GNeutralHeal1 extends GreenCard(CardEnum.G_NEUTRAL_HEAL1, "中立回一")
object GLifeOver12 extends GreenCard(CardEnum.G_LIFE_OVER12, "強者損二")
object GLifeUnder11 extends GreenCard(CardEnum.G_LIFE_UNDER11, "弱者損一")
object GRevealPrev extends GreenCard(CardEnum.G_REVEAL_PREV,     "角色顯示")
object GHunterHeal2 extends GreenCard(CardEnum.G_HUNTER_HEAL2,     "獵人回二")
object GLifeUnder11_2 extends GreenCard(CardEnum.G_LIFE_UNDER11_2, "弱者回二")

/*
 效果：我猜你是獵人，是的話請扣一滴血；我猜你是暗影，是的話請扣一滴血。
 效果：把你的角色卡偷偷給上一家的玩家看。
 效果：我猜你是獵人或暗影(中立或獵人、中立或暗影)，是的話請噴一張裝備給上一家，或是扣一滴血。
 效果：我猜你是獵人(中立、暗影)，是的話回復一滴血，如果你是滿血狀態，請扣一滴血。
 效果：我猜你的總血量高於12(低於11)，是的話請扣兩滴血(一滴血)。
 效果：我猜你是暗影，是的話扣兩滴血。(對已知身分的暗影很有殺傷力...)
 */
/*
 * Aid		Hunter heal 1 damage (if you have none take 1 damage)
 * Anger		Hunter or Shadow - give 1 equipment to current player or take 1 damage
 * Anger		Hunter or Shadow - give 1 equipment to current player or take 1 damage
 * Blackmail	Hunter or Neutral - give 1 equipment to current player or take 1 damage
 * Blackmail	Hunter or Neutral - give 1 equipment to current player or take 1 damage
 * Bully		HP less or equal 11 (A,B,C,E,U) take 1 damage
 * Exorcism	Shadow - take 2 damage
 * Greed		Neutral or Shadow - give 1 equipment to current player or take 1 damage
 * Greed		Neutral or Shadow - give 1 equipment to current player or take 1 damage
 * Huddle		Shadow heal 1 damage (if you have none take 1 damage)
 * Nurturance	Neutral heal 1 damage (if you have none take 1 damage)
 * Predicition	Show your character card to current player
 * Slap		Hunter - take 1 damage
 * Slap		Hunter - take 1 damage
 * Spell		Shadow - take 1 damage
 * Tough Lesson	HP greater or equal 12 (D,F,G,V,W) take 2 damage
 */