package org.plummtw.shadowhunter.data

import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.card._

class RoleData(val enum : RoleEnum.Value, val name : String, val life : Int, val side : RoleSideEnum.Value) {
  def role_enum = enum
  def role_name = name
  def role_life = life
  def role_side = side
  
  def cfield = {
    <span class={RoleSideEnum.get_roleside_color(role_side)}>[{role_name}]</span>
  }
  
  def simple_cfield = {
    <span class={RoleSideEnum.get_roleside_color(role_side)}>[{role_name.substring(0,1)}]</span>
  }
  
  def movement_skill : List[ActionData] = List(ActionNoAction)
  def attack_skill     : ActionData = ActionNoAction
  def free_skill      : ActionData = ActionNoAction
  def post_skill      : ActionData = ActionNoAction
}

class RoleShadow(override val enum : RoleEnum.Value, override val name : String, override val life : Int) extends RoleData(enum, name, life, RoleSideEnum.SHADOW)

class RoleHunter(override val enum : RoleEnum.Value, override val name : String, override val life : Int) extends RoleData(enum, name, life, RoleSideEnum.HUNTER)

class RoleNeutral(override val enum : RoleEnum.Value, override val name : String, override val life : Int) extends RoleData(enum, name, life, RoleSideEnum.NEUTRAL)

object RoleNone extends RoleData(RoleEnum.NONE, "不指定", 20, RoleSideEnum.NEUTRAL ) 
object RoleNoEffect extends RoleData(RoleEnum.NONE, "無效果", 20, RoleSideEnum.NONE ) 

// Shadow
object RoleUltraSoul extends RoleShadow(RoleEnum.ULTRASOUL, "究極靈魂", 11) {
  override def movement_skill = List(ActionUltrasoulRay, ActionUltrasoulUray)
}
object RoleUnknown extends RoleShadow(RoleEnum.UNKNOWN, "不明", 11) {
  override def free_skill = ActionUnknownDeceive
}
object RoleUnseen extends RoleShadow(RoleEnum.UNSEEN, "隱形人", 11)
object RoleUndead extends RoleShadow(RoleEnum.UNDEAD, "不死族", 11)

object RoleValkyrie extends RoleShadow(RoleEnum.VALKYRIE, "女武神", 13)
object RoleVampire extends RoleShadow(RoleEnum.VAMPIRE, "吸血鬼", 13)
object RoleVengefulGhost extends RoleShadow(RoleEnum.VENGEFUL_GHOST, "復仇鬼", 13)
object RoleViper extends RoleShadow(RoleEnum.VIPER, "毒蛇", 13)
object RoleWerewolf extends RoleShadow(RoleEnum.WEREWOLF, "狼人", 14) {
  override def free_skill = ActionWerewolfAmbush
}
object RoleWight extends RoleShadow(RoleEnum.WIGHT, "巫妖", 14) {
  override def post_skill = ActionWightManipulate
}
object RoleWitch extends RoleShadow(RoleEnum.WITCH, "女巫", 14)



object RoleWicked extends RoleShadow(RoleEnum.WICKED, "邪惡", 14)

// Hunter
object RoleEllen extends RoleHunter(RoleEnum.ELLEN, "艾蓮", 10) {
  override def movement_skill = List(ActionEllenCurseChain)
}
object RoleEmi extends RoleHunter(RoleEnum.EMI, "艾米", 10)
object RoleEmma extends RoleHunter(RoleEnum.EMMA, "艾瑪", 10)
object RoleEvan extends RoleHunter(RoleEnum.EVAN, "伊凡", 10) {
  override def movement_skill = List(ActionEvanBraceup)
}
object RoleFatherOconnel extends RoleHunter(RoleEnum.FATHER_OCONNEL, "歐肯奈", 12) {
  override def movement_skill = List(ActionFatherOconnelPray)
}

object RoleFranklin extends RoleHunter(RoleEnum.FRANKLIN, "弗蘭克林", 12) {
  override def movement_skill = List(ActionFranklinLightning)
}
object RoleFuka extends RoleHunter(RoleEnum.FUKA, "楓花", 12) {
  override def movement_skill = List(ActionFukaDynamiteHeal)
}
 
object RoleFeng extends RoleHunter(RoleEnum.FENG, "馮大師", 12) {
  override def attack_skill = ActionFengKikou
}

object RoleGeorge extends RoleHunter(RoleEnum.GEORGE, "喬治", 14) {
  override def movement_skill = List(ActionGeorgeDemolish)
}
object RoleGregor extends RoleHunter(RoleEnum.GREGOR, "葛瑞格", 14) {
  override def post_skill = ActionGregorBarrier
}

object RoleGinger extends RoleHunter(RoleEnum.GINGER, "金格", 14) 

object RoleGodfat extends RoleHunter(RoleEnum.GODFAT, "哥德法", 14) {
  override def movement_skill = List(ActionGodfatExchange)
}

// Neutral
object RoleAgnes extends RoleNeutral(RoleEnum.AGNES, "愛格妮絲", 8)
object RoleAllie extends RoleNeutral(RoleEnum.ALLIE, "愛莉", 8)  {
  override def movement_skill = List(ActionAllieMotherLove)
}
object RoleAngel extends RoleNeutral(RoleEnum.ANGEL, "天使", 8)  {
  override def free_skill = ActionAngelReincarnate
}
object RoleADecoy extends RoleNeutral(RoleEnum.ADECOY, "詛咒人偶", 8) {
  override def movement_skill = List(ActionADecoyTaunt)
}


object RoleBellandona extends RoleNeutral(RoleEnum.BELLANDONA, "貝爾多娜", 10)  {
}
object RoleBob extends RoleNeutral(RoleEnum.BOB, "鮑伯", 10)
object RoleBomb extends RoleNeutral(RoleEnum.BOMB, "波姆", 10) {
  override def movement_skill = List(ActionBombBomb)
}
object RoleBryan extends RoleNeutral(RoleEnum.BRYAN, "布萊恩", 10)
object RoleCatherine extends RoleNeutral(RoleEnum.CATHERINE, "凱瑟琳", 11)
object RoleCharles extends RoleNeutral(RoleEnum.CHARLES, "查理斯", 11) {
  override def post_skill = ActionCharlesBloodfeast
}
object RoleCassandra extends RoleNeutral(RoleEnum.CASSANDRA, "卡珊卓", 11) {
  override def movement_skill = List(ActionCassandraFateChange)
}
object RoleCheshire extends RoleNeutral(RoleEnum.CHESHIRE, "柴郡貓", 11) 


object RoleDavid extends RoleNeutral(RoleEnum.DAVID, "大衛", 13) {
  override def free_skill = ActionDavidGravedig
  
  def WIN_EQUIPMENT_LIST = List(WCardTalisman, 
    WCardHolyRobe, WCardSilverRosary, WCardLanceOfLonginus)

  def WIN_EQUIPMENT_LIST2 = List(WCardFortuneBrooch, WCardTalisman, 
    WCardHolyRobe, WCardSilverRosary, WCardLanceOfLonginus, WCardMysticCompass, 
    WCardBalance)
}


object RoleDaniel extends RoleNeutral(RoleEnum.DANIEL, "丹尼爾", 13)

object RoleDespair extends RoleNeutral(RoleEnum.DESPAIR, "絕望", 13)

object RoleDetective extends RoleNeutral(RoleEnum.DETECTIVE, "莉可", 13) {
  override def movement_skill = List(ActionDetectiveReasonA, ActionDetectiveReasonR)
}

