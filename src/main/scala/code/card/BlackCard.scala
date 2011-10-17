package org.plummtw.shadowhunter.card

import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.data._
import org.plummtw.shadowhunter.model._

class BlackCard(override val enum : CardEnum.Value, override val name : String) extends Card(enum, name, CardTypeEnum.BLACK)

object BCardChainsaw          extends BlackCard(CardEnum.B_CHAINSAW, "電鋸") with Equipment {
  override def equip_name = "鋸"
}
object BCardButcherKnife       extends BlackCard(CardEnum.B_BUTCHER_KNIFE, "菜刀") with Equipment {
  override def equip_name = "刀"
}
object BCardRustedBroadAxe    extends BlackCard(CardEnum.B_RUSTED_BROAD_AXE, "斧頭") with Equipment{
  override def equip_name = "斧"
}
object BCardVampireBat        extends BlackCard(CardEnum.B_VAMPIRE_BAT, "吸血蝙蝠") with UserEntryTargetable {
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    userentrys_rr.filter(x=>(x.id.is != currentuserentry.id.is) && (x.live.is) &&
	((currentuserentry.hasnt_user_flag(UserEntryFlagEnum.LOVER) || (x.hasnt_user_flag(UserEntryFlagEnum.LOVER)))))
  }
}
object BCardBloodthirstySpider  extends BlackCard(CardEnum.B_BLOODTHIRSTY_SPIDER, "嗜血蜘蛛") with UserEntryTargetable {
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    userentrys_rr.filter(x=>(x.id.is != currentuserentry.id.is) && (x.live.is) &&
	((currentuserentry.hasnt_user_flag(UserEntryFlagEnum.LOVER) || (x.hasnt_user_flag(UserEntryFlagEnum.LOVER)))))
  }
}
object BCardMoodyGoblin      extends BlackCard(CardEnum.B_MOODY_GOBLIN, "穆迪精靈") with UserEntryTargetable {
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    userentrys_rr.filter(x=>(x.id.is != currentuserentry.id.is) && (x.live.is) &&
	((currentuserentry.hasnt_user_flag(UserEntryFlagEnum.LOVER) || (x.hasnt_user_flag(UserEntryFlagEnum.LOVER)))))
  }
}
object BCardMasamune         extends BlackCard(CardEnum.B_MASAMUNE, "妖刀") with Equipment {
  override def equip_name = "妖"
}
object BCardMachineGune       extends BlackCard(CardEnum.B_MACHINE_GUN, "機槍") with Equipment {
  override def equip_name = "機"
}
object BCardHandGun          extends BlackCard(CardEnum.B_HANDGUN, "手槍") with Equipment {
  override def equip_name = "手"
}
object BCardSpiritualDoll       extends BlackCard(CardEnum.B_SPIRITUAL_DOLL, "心靈洋娃娃") with UserEntryTargetable {
  override def targetable_users(room: Room, roomround: RoomRound, roomphase:RoomPhase, currentuserentry : UserEntry, userentrys_rr : List[UserEntry]) : List[UserEntry] = {
    userentrys_rr.filter(x=>(x.id.is != currentuserentry.id.is) && (x.live.is) &&
	((currentuserentry.hasnt_user_flag(UserEntryFlagEnum.LOVER) || (x.hasnt_user_flag(UserEntryFlagEnum.LOVER)))))
  }
}
object BCardDynamite          extends BlackCard(CardEnum.B_DYNAMITE, "炸藥")
object BCardDiabolicRitual      extends BlackCard(CardEnum.B_DIABOLIC_RITUAL, "魔鬼儀式")
object BCardBananaPeel        extends BlackCard(CardEnum.B_BANANA_PEEL, "香蕉皮")

object BCardDagger             extends BlackCard(CardEnum.B_DAGGER, "匕首") with Equipment {
  override def equip_name = "匕"
}


/*
 * 電鋸、菜刀、斧頭(裝備卡)：

效果皆是攻擊力加一。

吸血蝙蝠：

指定一名玩家，扣兩格血量，回復自己一格血量。

嗜血蜘蛛：

指定一名玩家，扣兩格血量，自己也扣兩格血量。(注意，有時候會搞到自殺...)

穆迪精靈：

指定一名玩家，奪走他的一張裝備卡。

被詛咒的妖刀(裝備卡)：

攻擊時只需擲一面四面骰，強制攻擊。

機槍(裝備卡)：

攻擊時，同區域內的所有玩家都會受到攻擊。(抽到這張你就準備自己一個人一隊了...)

手槍(裝備卡)：

只能攻擊不同區域的玩家。

心靈洋娃娃：

指定一名玩家，擲出一面六面骰，數字1~4對方扣3格血，數字5、6自己扣3格血。

炸藥：

同時擲六面骰跟四面骰，擲出的數字總合，相同數字的場地裡面，所有玩家扣3格血。骰到無人區域或是7就無效丟棄。

魔鬼儀式：

如果你是暗影，可以亮出你的身分，回復所有血量。

香蕉王皮：

踩到香蕉皮，必須噴一張裝備卡給上一家，沒有裝備卡就扣一滴血。
(全套牌僅有的一張香蕉皮，誰踩到了誰就是香蕉王!!)

 */