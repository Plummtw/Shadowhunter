package org.plummtw.shadowhunter.card

import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.data._

class WhiteCard(override val enum : CardEnum.Value, override val name : String) extends Card(enum, name, CardTypeEnum.WHITE)


object WCardHolyWaterOfHealing extends WhiteCard(CardEnum.W_HOLY_WATER_OF_HEALING, "治療聖水")
object WCardTalisman           extends WhiteCard(CardEnum.W_TALISMAN, "護身符") with Equipment {
  override def equip_name = "符"
}
object WCardFortuneBrooch      extends WhiteCard(CardEnum.W_FORTUNE_BROOCH, "財富胸針")  with Equipment {
  override def equip_name = "針"
}
object WCardMysticCompass     extends WhiteCard(CardEnum.W_MYSTIC_COMPASS, "神秘羅盤") with Equipment {
  override def equip_name = "盤"
}
object WCardHolyRobe          extends WhiteCard(CardEnum.W_HOLY_ROBE, "神聖長袍") with Equipment {
  override def equip_name = "袍"
}
object WCardSilverRosary        extends WhiteCard(CardEnum.W_SILVER_ROSARY, "白銀十字架") with Equipment {
  override def equip_name = "架"
}
object WCardLanceOfLonginus    extends WhiteCard(CardEnum.W_LANCE_OF_LONGINUS, "朗基努斯槍") with Equipment {
  override def equip_name = "槍"
}
object WCardAdvent             extends WhiteCard(CardEnum.W_ADVENT, "降臨")
object WCardChocolate          extends WhiteCard(CardEnum.W_CHOCOLATE, "巧克力")
object WCardBlessing          extends WhiteCard(CardEnum.W_BLESSING, "祝福") with UserEntryTargetable {
  
}
object WCardConcealedKnowledge extends WhiteCard(CardEnum.W_CONCEALED_KNOWLEDGE, "隱藏的智慧")
object WCardGuardianAngel      extends WhiteCard(CardEnum.W_GUARDIAN_ANGEL, "守護天使")
object WCardFlareOfJudgement  extends WhiteCard(CardEnum.W_FLARE_OF_JUDGEMENT, "閃電制裁")
object WCardDisenchantedMirror extends WhiteCard(CardEnum.W_DISENCHANTED_MIRROR, "照妖鏡")
object WCardFirstAid          extends WhiteCard(CardEnum.W_FIRST_AID, "急救箱") with UserEntryTargetable {
  override def targetable_users(room:Room, roomround:RoomRound, roomphase:RoomPhase, currentuserentry:UserEntry, userentrys_rr:List[UserEntry]) : List[UserEntry] = {
    //val userentrys = UserEntrys_RR.get
    userentrys_rr.filter(x=> (x.live.is) )
  }
}

object WCardTea extends WhiteCard(CardEnum.W_TEA, "分解茶")
/*
 * 治療聖水：

回復兩格血。

護身符、財富胸針(裝備卡)：

護身符：
抗黑卡的-血腥蜘蛛、吸血蝙蝠、炸藥。
財富胸針：
抗怪異樹林的效果。(但是可以對自己使用)

神秘羅盤、神聖長袍、白銀十字架(裝備卡)：

神秘羅盤：移動階段，可以擲兩次骰子，選擇自己想要的一個數字移動。
神聖長袍：受到的傷害減一，輸出的傷害也減一。
白銀十字架：殺死一名玩家，可以獲得他所有的裝備卡。

朗基努斯之槍(裝備卡)：

獵人裝備可以增加兩點傷害，需亮身分。(只要裝上他，蘿莉也可以變成血腥蘿莉!!)

降臨：

如果你是獵人，亮出你的身分，回復所有血量。

巧克力：

如果你是愛莉、艾米、不明，亮出身分，回復所有血量。

祝福：

選擇一個你以外的玩家，擲一個六面骰，回復點數所骰到的血量。

隱藏的智慧：

你結束這回合時，可以在繼續一次回合。

守護天使：

你這回合開始，直到下個你的回合，都不會受到物理傷害。(開毫髮的意思)

閃電判裁：

除了使用者，其他玩家皆扣兩格血量。

照妖鏡：

如果使用者是：吸血鬼、狼人，就必須亮出身分。

急救箱：

選擇一個玩家(自己可)，直接將他移動到第七格。(只要使用得當，可以救人也可以殺人。)

 */
