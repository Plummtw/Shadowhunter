package org.plummtw.shadowhunter.heavy

import scala.xml._
import net.liftweb._
import net.liftweb.mapper._
import http._
import js._
import util._
import common._
import S._
import SHtml._
import Helpers._

//import scala.collection.JavaConversions
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer 

import org.plummtw.shadowhunter.model._
import org.plummtw.shadowhunter.enum._
import org.plummtw.shadowhunter.actor._
import org.plummtw.shadowhunter.data._
import org.plummtw.shadowhunter.card._
import org.plummtw.shadowhunter.util.PlummUtil

object GameProcessor extends Logger{
  val random = scala.util.Random
  
  def shuffle_cardpool(room : Room, card_type : CardTypeEnum.Value, card_pool : List[CardPool]) {
    //val room = Room_R.get
    //val card_pool = CardPool.findAll(By(CardPool.room_id, room.id.is),
    //                              By(CardPool.card_type, card_type.toString))
    
    if (card_pool.length == 0) {
      val card_list = card_type match {
        case CardTypeEnum.BLACK => 
          var result = CardEnum.BLACK_LIST
          if (room.has_flag(RoomFlagEnum.BLACKCARD_DAGGER))
            result = CardEnum.B_DAGGER :: result
          if (room.has_flag(RoomFlagEnum.BLACKCARD_LAMIRROR))
            result = CardEnum.B_LAMIRROR :: result
          result
        case CardTypeEnum.WHITE => 
          var result = CardEnum.WHITE_LIST
          if (room.has_flag(RoomFlagEnum.WHITECARD_TEA))
            result = CardEnum.W_TEA :: result
          result
        case CardTypeEnum.GREEN => 
          var result = CardEnum.GREEN_LIST
          if (room.has_flag(RoomFlagEnum.GREENCARD_HUNTERHEAL2))
            result = CardEnum.G_HUNTER_HEAL2 :: result
          if (room.has_flag(RoomFlagEnum.GREENCARD_LIFEUNDER11_2))
            result = CardEnum.G_LIFE_UNDER11_2 :: result
          result
      }
      
      val java_card_list: java.util.List[CardEnum.Value] = ListBuffer(card_list: _*)
      
      java.util.Collections.shuffle(java_card_list)
      
      val card_list2 = java_card_list.toList
      var card_no = 0
      card_list2 foreach { card_data =>
        val card = CardPool.create.room_id(room.id.is).card_no(card_no).card_type(card_type.toString)
                           .card(card_data.toString).discarded(false)
        card.save
        card_no = card_no + 1
      }
    } else {
      val java_card_no_list: java.util.List[Int] = new java.util.ArrayList()
      for (i <- 0 until card_pool.length) 
        java_card_no_list.add(i)  
      
      java.util.Collections.shuffle(java_card_no_list)
      
      var card_index = 0
      card_pool foreach { card =>
        if (card.owner_id.is == 0)
          card.discarded(false)
        card.card_no(java_card_no_list.get(card_index))
        card.save
        card_index = card_index + 1
      }
    }
  }
  
  def draw_card(room : Room, card_type : CardTypeEnum.Value) : CardPool = {
    //val room = Room_R.get
    var card_list = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                     OrderBy(CardPool.card_no, Ascending))
    var card_pool = card_list.filter(x => x.card_type.is == card_type.toString)
    
    var card_index = card_type match {
      case CardTypeEnum.BLACK => room.blackcard_index.is
      case CardTypeEnum.WHITE => room.whitecard_index.is
      case CardTypeEnum.GREEN => room.greencard_index.is
    }
    
    var result : CardPool = null
    do {
      if (card_index >= card_pool.length) {
        shuffle_cardpool(room, card_type, card_pool)
        card_index = 0
      
        card_list = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                     OrderBy(CardPool.card_no, Ascending))
        card_pool = card_list.filter(x => x.card_type.is == card_type.toString)
        //card_pool = CardPool.findAll(By(CardPool.room_id, room.id.is),
        //                            By(CardPool.card_type, card_type.toString))
      }
    
      result = card_pool(card_index)
      card_index = card_index + 1
    } while ((result.discarded.is) || (result.owner_id.is != 0))
    
    if (!CardEnum.get_card(result.card.is).isInstanceOf[Equipment])
      result.discarded(true).save
    

    card_type match {
      case CardTypeEnum.BLACK => room.blackcard_index(card_index)
      case CardTypeEnum.WHITE => room.whitecard_index(card_index)
      case CardTypeEnum.GREEN => room.greencard_index(card_index)
    }
    
    room.save
    
    RoomActor ! SessionVarSet(room = room, card_list = card_list)
    RoomActor ! RoomForceUpdate(room.id.is, List(ForceUpdateEnum.CARD_TABLE))
    
    result
  }
  
  def draw_specific(room : Room, card_str : String) : CardPool = {
    
    val card_id = try {
      card_str.toLong 
    } catch { case e : Exception => 0L}
    
    //val room = Room_R.get
    val card_list = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                        OrderBy(CardPool.card_no, Ascending))
    //val card_pool = card_list.filter(x => x.card_type.is == card_type.toString)

    val result = card_list.filter(_.id.is == card_id)(0)
    
    if (!CardEnum.get_card(result.card.is).isInstanceOf[Equipment])
      result.discarded(true).save
    

    RoomActor ! SessionVarSet(room = room, card_list = card_list)
    RoomActor ! RoomForceUpdate(room.id.is, List(ForceUpdateEnum.CARD_TABLE))
    
    result
  }
  
  def peek_card2(room : Room, card_type : CardTypeEnum.Value) : (CardPool, CardPool) = {
    //val room = Room_R.get
    var card_list = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                     OrderBy(CardPool.card_no, Ascending))
    var card_pool = card_list.filter(x => x.card_type.is == card_type.toString)
    
    var card_index = card_type match {
      case CardTypeEnum.BLACK => room.blackcard_index.is
      case CardTypeEnum.WHITE => room.whitecard_index.is
      case CardTypeEnum.GREEN => room.greencard_index.is
    }
    
    var result : CardPool = null
    if (card_index >= card_pool.length) {
      shuffle_cardpool(room, card_type, card_pool)
      card_index = 0
      
      card_list = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                   OrderBy(CardPool.card_no, Ascending))
      card_pool = card_list.filter(x => x.card_type.is == card_type.toString)
    }
    
    var card_pool2 = card_pool.filter(x => (x.card_no.is >= card_index) &&
                                           (!x.discarded.is) && (x.owner_id.is == 0))
    
    if (card_pool2.length >= 2)
      ((card_pool2(0), card_pool2(1)))
    else if (card_pool2.length == 1)
      ((card_pool2(0), card_pool2(0)))
    else {
      shuffle_cardpool(room, card_type, card_pool)
      card_index = 0
      
      card_list = CardPool.findAll(By(CardPool.room_id, room.id.is),
                                   OrderBy(CardPool.card_no, Ascending))
      card_pool = card_list.filter(x => x.card_type.is == card_type.toString)
      card_pool2 = card_pool.filter(x => (x.card_no.is >= card_index) &&
                                           (!x.discarded.is) && (x.owner_id.is == 0))
      card_type match {
        case CardTypeEnum.BLACK => room.blackcard_index(card_index)
        case CardTypeEnum.WHITE => room.whitecard_index(card_index)
        case CardTypeEnum.GREEN => room.greencard_index(card_index)
      }
    
      room.save
    
      RoomActor ! SessionVarSet(room = room, card_list = card_list)
      RoomActor ! RoomForceUpdate(room.id.is, List(ForceUpdateEnum.CARD_TABLE))

      ((card_pool2(0), card_pool2(1)))
    }
  }
  
  // 分配職業
  def dispatch_role(room : Room, userentrys : List[UserEntry]) {
    val random = new scala.util.Random
        
    val shadow_hunter_number = if (userentrys.length>=8) 3 else 2
    val neutral_number       = userentrys.length - 2 * shadow_hunter_number
    
    // 先產生職業清單
    /*
    var all_role_list = RoleEnum.STANDARD_ROLE_LIST
    if (room.has_flag(RoomFlagEnum.EXPANSION_ROLE))
      all_role_list :::= RoleEnum.EXPANSION_ROLE_LIST
    if (room.has_flag(RoomFlagEnum.CUSTOM_ROLE))
      all_role_list :::= RoleEnum.CUSTOM_ROLE_LIST
    */
   
    var all_role_list = List(
      RoleEnum.VAMPIRE, RoleEnum.WEREWOLF, RoleEnum.EMI, RoleEnum.FRANKLIN, RoleEnum.GEORGE,
      RoleEnum.ALLIE, RoleEnum.BOB, RoleEnum.DANIEL, RoleEnum.ULTRASOUL, RoleEnum.VALKYRIE,
      RoleEnum.AGNES, RoleEnum.BRYAN, RoleEnum.CATHERINE, RoleEnum.ELLEN, RoleEnum.FUKA,
      RoleEnum.WIGHT, RoleEnum.GREGOR, RoleEnum.CHARLES, RoleEnum.UNSEEN, RoleEnum.VENGEFUL_GHOST,
      RoleEnum.GINGER, RoleEnum.UNKNOWN, RoleEnum.DAVID, RoleEnum.FATHER_OCONNEL, RoleEnum.WITCH,
      RoleEnum.ANGEL,  RoleEnum.BELLANDONA, RoleEnum.EVAN, RoleEnum.CASSANDRA, RoleEnum.DESPAIR,
      RoleEnum.BOMB, RoleEnum.EMMA, RoleEnum.FENG, RoleEnum.GODFAT,
          RoleEnum.UNDEAD, RoleEnum.VIPER, RoleEnum.WICKED, RoleEnum.ADECOY, RoleEnum.CHESHIRE,
        RoleEnum.DETECTIVE)
    
    var java_shadow_list: java.util.List[RoleEnum.Value] = 
      ListBuffer(all_role_list.filter(RoleEnum.get_role(_).role_side == RoleSideEnum.SHADOW): _*)
    var java_hunter_list: java.util.List[RoleEnum.Value] = 
      ListBuffer(all_role_list.filter(RoleEnum.get_role(_).role_side == RoleSideEnum.HUNTER): _*)
    var java_neutral_list: java.util.List[RoleEnum.Value] = 
      ListBuffer(all_role_list.filter(RoleEnum.get_role(_).role_side == RoleSideEnum.NEUTRAL): _*)
    
    java.util.Collections.shuffle(java_shadow_list)
    java.util.Collections.shuffle(java_hunter_list)
    java.util.Collections.shuffle(java_neutral_list)
    
    java_shadow_list = java_shadow_list.take(shadow_hunter_number)
    java_hunter_list = java_hunter_list.take(shadow_hunter_number)
    java_neutral_list = java_neutral_list.take(neutral_number)
    
    //println("shadow : " + java_shadow_list.size())
    //println("hunter : " + java_hunter_list.size())
    //println("neutral : " + java_neutral_list.size())
    
    // 設定玩家優先順位
    var java_user_no_array : java.util.LinkedList[Int] = new java.util.LinkedList()
    for (i <- 1 to userentrys.length)
      java_user_no_array.add(i)
      
    java.util.Collections.shuffle(java_user_no_array)
    
    userentrys.foreach { userentry =>
      userentry.user_no(java_user_no_array.removeFirst()).room_flags(userentry.role.is).role("")
      //println(user_entry.user_no.is + " " + user_entry.user_flags.is)
    }

    val userentrys_ordered = userentrys.sortBy(_.user_no.is)

    // 第一次先看看有沒有希望陣營
    userentrys_ordered.foreach { userentry =>
      val role  = try { RoleEnum.withName(userentry.room_flags.is) }
        catch { case e: Exception => RoleEnum.NONE }
      val align = try { RoleSideEnum.withName(userentry.subrole.is) }
        catch { case e: Exception => RoleSideEnum.NONE }
      if ((role != RoleEnum.NONE) && (random.nextInt(6) != 0)) {  
        val align_array = RoleEnum.get_role(role).role_side match {
          case RoleSideEnum.SHADOW => java_shadow_list
          case RoleSideEnum.HUNTER => java_hunter_list
          case RoleSideEnum.NEUTRAL => java_neutral_list
        }
        if (align_array.contains(role)) {
          userentry.role(role.toString)
          align_array.remove(role)
        }
      } else if ((align != RoleSideEnum.NONE) && (random.nextInt(6) != 0)) {
        val align_array = align match {
          case RoleSideEnum.SHADOW => java_shadow_list
          case RoleSideEnum.HUNTER => java_hunter_list
          case RoleSideEnum.NEUTRAL => java_neutral_list
        }
        if (align_array.size() != 0) {
          userentry.role(align_array.get(0).toString)
          align_array.remove(0)
        }
      }
    }

    // 然後設定剩下的職業
    var java_role_array : java.util.LinkedList[RoleEnum.Value] = new java.util.LinkedList()
    java_role_array.addAll(java_shadow_list)
    java_role_array.addAll(java_hunter_list)
    java_role_array.addAll(java_neutral_list)
    
    java.util.Collections.shuffle(java_role_array)

    userentrys_ordered.foreach { userentry =>
      if (userentry.role.is == "") {
        userentry.role(java_role_array.removeFirst().toString)
      }
    }
    
    if (room.has_flag(RoomFlagEnum.INIT_LOCATION)) {
      userentrys_ordered.foreach { userentry =>
        val index = random.nextInt(LocationEnum.LOCATION_LIST.length)
        userentry.location(LocationEnum.LOCATION_LIST(index).toString)
      }
    }
    
    //java_user_no_array : java.util.LinkedList[Int] = new java.util.LinkedList()
    for (i <- 1 to userentrys.length)
      java_user_no_array.add(i)
    
    if (room.has_flag(RoomFlagEnum.RANDOM_POSITION))
      java.util.Collections.shuffle(java_user_no_array)
    
    userentrys.foreach { userentry =>
      userentry.user_no(java_user_no_array.removeFirst()).room_flags("").damaged(0).subrole("")
      if (userentry.role.is != RoleEnum.UNKNOWN.toString)
        userentry.role_flags("")
      //println(user_entry.user_no.is + " " + user_entry.user_flags.is)
      userentry.save
    }
  }
  
  def process_start_game(room : Room) = {
    // val room = Room.find(By(Room.id, room_id)).get
    var userentrys_rr = UserEntry.findAll(By(UserEntry.room_id, room.id.is),
                                        By(UserEntry.revoked, false))
    
    dispatch_role(room, userentrys_rr)
    
    // 產生亂數地形
    val location_list = new java.util.LinkedList[LocationEnum.Value]()
    LocationEnum.LOCATION_LIST.foreach { location =>
      location_list.add(location)
    }
    java.util.Collections.shuffle(location_list)
    room.room_arrange(location_list.toList.map(_.toString).mkString(""))
    
    if (room.has_flag(RoomFlagEnum.RANDOM_POSITION))
      userentrys_rr = UserEntry.findAll(By(UserEntry.room_id, room.id.is),
                                        By(UserEntry.revoked, false),
                                        OrderBy(UserEntry.user_no, Ascending))
    
    // 加入第一回合
    val new_round = RoomRound.create.room_id(room.id.is).round_no(1)
    new_round.save
    
    val new_phase = RoomPhase.create.roomround_id(new_round.id.is)
                    .phase_type(RoomPhaseEnum.MOVEMENT.toString)
                    .player(userentrys_rr(0).id.is)
                    .deadline(PlummUtil.dateAddSecond(new java.util.Date(), room.move_time))
    new_phase.save

    // 產生人數字串
    val align_text = new StringBuffer("陣營分布：")
    align_text.append("　暗影：")
    align_text.append(userentrys_rr.filter(x => RoleEnum.get_role(x.role.is).role_side == RoleSideEnum.SHADOW).length)
    align_text.append("　獵人：")
    align_text.append(userentrys_rr.filter(x => RoleEnum.get_role(x.role.is).role_side == RoleSideEnum.HUNTER).length)
    align_text.append("　中立：")
    align_text.append(userentrys_rr.filter(x => RoleEnum.get_role(x.role.is).role_side == RoleSideEnum.NEUTRAL).length)

    val talk = Talk.create.roomround_id(new_round.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                          .message("第 " + (new_round.round_no.is.toString) + " 日 "+ (new java.util.Date).toString)
    talk.save
    
    val talk2 = Talk.create.roomround_id(new_round.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                          .message(align_text.toString).cssclass("normal")
    talk2.save
    //RoomActor ! NewMessageNoSend(talk)
    
    if (room.has_flag(RoomFlagEnum.INIT_GREEN)) {
      userentrys_rr.foreach { userentry =>
        val index = userentrys_rr.indexOf(userentry)
        val prev_index = (index + userentrys_rr.length - 1) % (userentrys_rr.length)
        val targetuserentry = userentrys_rr(prev_index)
        
        val card = draw_card(room, CardTypeEnum.GREEN)
        
        val talk3 = Talk.create.roomround_id(new_round.id.is).mtype(MTypeEnum.ACTION_GREENCARD.toString)
                       .actioner_id(userentry.id.is).actionee_id(targetuserentry.id.is).message_flags(card.card.is.toString)
        talk3.save
        
        val talk4 = CardHelper.process_green_internal(userentry, targetuserentry, CardEnum.get_card(card.card.is.toString))
        talk4.roomround_id(new_round.id.is).save
        
        targetuserentry.save
      }
    }

    room.status(RoomStatusEnum.PLAYING.toString)
    room.save
    
    //RoomActor ! NewRoomRound(room_i, new_round)
  }
  
  def check_death(actionee : UserEntry, actioner : UserEntry, action : Action, userentrys : List[UserEntry]) : Boolean = {
    val role = actionee.get_role
    //val actioner = userentrys.filter(_.id.is == action.actioner_id.is)(0)
    var actioner_role = actioner.get_role
    
    val is_dead =
      if ((role == RoleUnknown) && (!actionee.revealed.is) && (actionee.damaged.is < 14) &&
          (actionee.hasnt_user_flag(UserEntryFlagEnum.SEALED)))
        false
      else  if (actionee.damaged.is >= role.life)
        true
      else false
      
    if (is_dead) {
      val actionee_equips = actionee.items
      if (actionee_equips.length > 0) {
        if (actioner == actionee) {
          // 剩下的裝備丟至墓地
          actionee.items.foreach { actionee_item =>
            val card = CardPool.find(By(CardPool.room_id, actionee.room_id.is),
                                     By(CardPool.card, actionee_item.card_enum.toString)).get
            card.owner_id(0).discarded(true).save
          }
      
          actionee.item_flags("").save
        } else
          rob(actioner, actionee)
        //check_item_victory(actioner)
      }
      
      val dead_userentrys = userentrys.filter (x => (!x.revoked.is) && (!x.live.is) && (x.get_role != RoleAngel))
      if ((role == RoleAngel) && (dead_userentrys.length != 0)) {
        val target_player_index = 
          dead_userentrys.map(_.id.is).indexOf(actionee.target_user.is)
        val dead_useretrys_index = 
          if (target_player_index == -1)
            random.nextInt(dead_userentrys.length)
          else
            target_player_index
          
        val dead_userentry = dead_userentrys(dead_useretrys_index)
        
        actionee.role(dead_userentry.role.is.substring(0,2) + actionee.role.is)
        actionee.target_user(dead_userentry.target_user.is) // role_flags(dead_userentry.role_flags.is).
        actionee.damaged(7)
        
        if (!actionee.revealed.is)
          GameProcessor.flip(actionee, action, userentrys)
      } else {
        if ((actioner_role == RoleBellandona) && (actioner != actionee)) {
          actioner.role(actionee.role.is.substring(0,2) + actioner.role.is)
          actioner.target_user(actionee.target_user.is) // role_flags(actionee.role_flags.is)
          actioner_role = actionee.get_role
        
          val actionee_role = actionee.get_role
          if (actioner.damaged.is >= actionee_role.role_life)
            actioner.damaged(actionee_role.role_life - 1)

          //GameProcessor.check_death(actioner, actioner, action, userentrys)
          actioner.save
        }
        
        if ((role == RoleDaniel) || (role == RoleCatherine)) {
          val userentrys_dead = userentrys.filter(x => (!x.revoked.is) && (!x.live.is) && (x.hasnt_user_flag(UserEntryFlagEnum.REVIVED)))
          if (userentrys_dead.length == 0) {
            actionee.add_user_flag(UserEntryFlagEnum.VICTORY) 
          }
        } else if (role == RoleDespair) {
          userentrys.foreach { userentry =>
            check_item_victory(userentry)
            if (userentry.live.is && userentry.has_user_flag(UserEntryFlagEnum.VICTORY2))
              userentry.add_user_flag(UserEntryFlagEnum.VICTORY)
          }
        } else if (role == RoleBomb) {
          val death_number = userentrys.filter(x => !x.live.is).length
          userentrys.foreach { userentry1 =>
            if ((userentry1.location.is == actionee.location.is) &&
                (userentry1.id.is != actionee.id.is) &&
                (userentry1.get_role != RoleBomb))
              if (userentry1.inflict_damage(3, actionee))
                GameProcessor.check_death(userentry1, actioner, action, userentrys)
          }
          val death_number2 = userentrys.filter(x => !x.live.is).length
          if (death_number2 - death_number >= 1)
            actionee.add_user_flag(UserEntryFlagEnum.VICTORY)
        }
      
        if (actioner_role == RoleBryan) {
          if (role.role_life >= 13) {
            actioner.add_user_flag(UserEntryFlagEnum.VICTORY) 
            actioner.save
          } else if ((!actioner.revealed) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED))) {
            flip(actioner, action, userentrys)
          }
        } else if (actioner_role == RoleCharles) {
          val userentrys_length = userentrys.filter(x => !x.revoked.is).length
          val dead_char_length =
            if (userentrys_length <=5)
              1
            else if (userentrys_length >=9)
              3
            else
              2
          
          val userentrys_dead = userentrys.filter(x => (!x.revoked.is) && (!x.live.is))
          if (userentrys_dead.length >= dead_char_length)
            actioner.add_user_flag(UserEntryFlagEnum.VICTORY) 
        }
      
        val live_daniels = userentrys.filter(x => (x.id.is != actionee.id.is) &&
          (!x.revoked.is) && (x.live.is) && (x.get_role == RoleDaniel) && (x.hasnt_user_flag(UserEntryFlagEnum.SEALED) && (!x.revealed.is)))
        live_daniels.foreach { live_daniel =>
          val action1 = Action.create.roomround_id(action.roomround_id.is).actioner_id(live_daniel.id.is)
                              .mtype(MTypeEnum.ACTION_FLIP.toString)
          action1.save
          val talk = Talk.create.roomround_id(action.roomround_id.is).actioner_id(live_daniel.id.is)
                                 .mtype(MTypeEnum.ACTION_FLIP.toString)
          talk.save
          talk.send(live_daniel.room_id.is)
        
          if (live_daniel.damaged.is < 5)
            live_daniel.damaged(5)
          
          live_daniel.revealed(true)
          live_daniel.save
        }
        
        actionee.live(false).location("")
        if (!actionee.revealed.is)
          GameProcessor.flip(actionee, action, userentrys)

        val talk1 = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actionee.id.is)
                               .mtype(MTypeEnum.MESSAGE_DEATH.toString)
        talk1.save
        talk1.send(actionee.room_id.is)
        
        val live_cheshires = userentrys.filter(x => (x.get_role == RoleCheshire) && (x.live.is) &&
                                                 (x.target_user.is == actionee.id.is))
        live_cheshires.foreach { live_cheshire =>
          if (role.role_side == RoleSideEnum.NEUTRAL) {
            val saved_damaged = live_cheshire.damaged.is
            live_cheshire.damaged(99)
            GameProcessor.check_death(live_cheshire, live_cheshire, action, userentrys)
            live_cheshire.damaged(saved_damaged).save
          } else if (!live_cheshire.revealed.is)
            GameProcessor.flip(live_cheshire, action, userentrys)
        }
        
        val live_unrevealed = userentrys.filter(x => (!x.revoked.is) && (x.get_role != RoleDetective) && (x.live.is) && (!x.revealed.is))
        if (live_unrevealed.length == 0) {
          val live_detectives = userentrys.filter(x =>(x.get_role == RoleDetective) && (x.live.is))
          live_detectives.foreach { live_detective =>
            val saved_damaged = live_detective.damaged.is
            live_detective.damaged(99)
            GameProcessor.check_death(live_detective, live_detective, action, userentrys)
            live_detective.damaged(saved_damaged).save
          }
        }

      }
    }  
    actionee.save
    
    !actionee.live.is
  }
  
  def rob_single(actioner: UserEntry, actionee: UserEntry) = {
    // 注意: actionee 須手動 save
    val actioner_role = actioner.get_role
    val actionee_equips = actionee.items
    
    val equip_index = random.nextInt(actionee_equips.length)
    val equip =
      if (actioner_role == RoleDavid) {
        val david_equips1 = actionee_equips.filter( x=> RoleDavid.WIN_EQUIPMENT_LIST.contains(x))
        val david_equips2 = actionee_equips.filter( x=> RoleDavid.WIN_EQUIPMENT_LIST2.contains(x))
        if (david_equips1.length != 0)
          david_equips1(0)
        else if (david_equips2.length != 0)
          david_equips2(0)
        else
          actionee_equips(equip_index)
      }  
      else actionee_equips(equip_index)

    actioner.add_item(equip.card_enum).save
    actionee.remove_item(equip.card_enum)
    
    // 從 CardPool 修改 owner 資訊
    val card = CardPool.find(By(CardPool.room_id, actionee.room_id.is),
                             By(CardPool.card, equip.card_enum.toString)).get
    card.owner_id(actioner.id.is).discarded(false).save
    
    check_item_victory(actioner)
    
    equip
  }
  
  def rob_specific(actioner: UserEntry, actionee: UserEntry, item : Card) = {
    // 注意: actionee 須手動 save

    actioner.add_item(item.card_enum).save
    actionee.remove_item(item.card_enum)
    
    // 從 CardPool 修改 owner 資訊
    val card = CardPool.find(By(CardPool.room_id, actionee.room_id.is),
                             By(CardPool.card, item.card_enum.toString)).get
    card.owner_id(actioner.id.is).discarded(false).save
    
    check_item_victory(actioner)
    
    item
  }
  
  def rob(actioner: UserEntry, actionee: UserEntry) {
    // 注意: actionee 須手動 save
    val actioner_role = actioner.get_role
    val actionee_equips = actionee.items
    
    if (!actioner.live.is) {
    } else if (((actioner_role == RoleBob) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED))) ||
        (actioner.has_item(CardEnum.W_SILVER_ROSARY))) {

      actioner.item_flags(actioner.item_flags.is.toString + actionee.item_flags.is.toString).save
      
      // 從 CardPool 修改 owner 資訊
      actionee.items.foreach { actionee_item =>
        val card = CardPool.find(By(CardPool.room_id, actionee.room_id.is),
                                 By(CardPool.card, actionee_item.card_enum.toString)).get
        card.owner_id(actioner.id.is).save
      }
      
      check_item_victory(actioner)
      actionee.item_flags("").save
    } else {
      rob_single(actioner, actionee)
      
      // 剩下的裝備丟至墓地
      actionee.items.foreach { actionee_item =>
        val card = CardPool.find(By(CardPool.room_id, actionee.room_id.is),
                                 By(CardPool.card, actionee_item.card_enum.toString)).get
        card.owner_id(0).discarded(true).save
      }
      
      actionee.item_flags("").save
    }
      
  }
  
  def check_item_victory(actioner : UserEntry) = {
    var result = false
    val actioner_role = actioner.get_role
    if (actioner_role == RoleBob) {
      val actioner_equips = actioner.items
      if ((actioner_equips.length >= 5) ||
          ((actioner_equips.length >= 4) && (!actioner.revealed.is))) {
        actioner.add_user_flag(UserEntryFlagEnum.VICTORY).save 
        result = true
      }
    } else if (actioner_role == RoleDavid) {
      val win_items = actioner.items.filter( x=> RoleDavid.WIN_EQUIPMENT_LIST.contains(x))
      if (win_items.length >= 3) {
        actioner.add_user_flag(UserEntryFlagEnum.VICTORY).save 
        result = true
      } else {
        val win_items2 = actioner.items.filter( x=> RoleDavid.WIN_EQUIPMENT_LIST2.contains(x))
        if (win_items2.length >= 4) {
          actioner.add_user_flag(UserEntryFlagEnum.VICTORY).save 
          result = true
        }
      }
    }
    result
  }
  
  
  def check_victory(room: Room, roomround: RoomRound, userentrys : List[UserEntry]) : Boolean = {
    if (room.status.is == RoomStatusEnum.ENDED)
      return true
    
    val userentrys_r  = userentrys.filter(x => (!x.revoked.is))
    val userentrys_rl = userentrys_r.filter(x => (x.live.is))
    
    // 檢查遊戲是否結束
    val userentrys_r_shadow = userentrys_rl.filter(x => (x.get_role.role_side == RoleSideEnum.SHADOW))
    val userentrys_r_hunter = userentrys_rl.filter(x => (x.get_role.role_side == RoleSideEnum.HUNTER))
    val userentrys_d_neutral = userentrys_r.filter(x =>  (!x.live.is) && (x.get_role.role_side == RoleSideEnum.NEUTRAL))
    val userentrys_l         = userentrys_rl.filter(x => x.has_user_flag(UserEntryFlagEnum.LOVER))
    val userentrys_v = userentrys_r.filter(x => (x.has_user_flag(UserEntryFlagEnum.VICTORY)))
    
    val is_shadow_victory_nd =
      if (room.has_flag(RoomFlagEnum.FOUR_NEUTRAL) && (userentrys_r.length == 10))
        (userentrys_d_neutral.length >= 4)
      else
        (userentrys_d_neutral.length >= 3)
        
    var is_hunter_victory = (userentrys_r_shadow.length == 0)
    var is_shadow_victory = (userentrys_r_hunter.length == 0) || is_shadow_victory_nd
    var is_neutral_victory = (userentrys_v.length > 0)
    var is_lover_victory = (userentrys_l.length >= 2) && 
      (userentrys_rl.filter(x => ((x.get_role.role_side == RoleSideEnum.SHADOW) ||
                                  (x.get_role.role_side == RoleSideEnum.HUNTER))).length == userentrys_l.length)
    
    val live_despairs = userentrys_rl.filter(x => (x.get_role == RoleDespair) && (x.revealed.is))
    if (live_despairs.length != 0) {
      is_hunter_victory = false
      is_shadow_victory = false
      is_lover_victory  = false
      
      if (userentrys_rl.length == 1) {
        val live_despair = live_despairs(0)
        live_despair.add_user_flag(UserEntryFlagEnum.VICTORY)
        live_despair.save
        is_neutral_victory = true
      } else {
        userentrys_v.foreach { userentry =>
          userentry.remove_user_flag(UserEntryFlagEnum.VICTORY)
          userentry.save
          is_neutral_victory = false
        }
      }
    }
    
    if (is_hunter_victory || is_shadow_victory || is_neutral_victory || is_lover_victory) {
      val victory = 
        if (is_lover_victory)
          RoomVictoryEnum.LOVER_WIN
        else if (is_hunter_victory && is_shadow_victory)
          RoomVictoryEnum.DUAL_WIN
        else if (is_hunter_victory)
          RoomVictoryEnum.HUNTER_WIN
        else if (is_shadow_victory)
          RoomVictoryEnum.SHADOW_WIN
        else
          RoomVictoryEnum.NEUTRAL_WIN
        
      // 再次測試是否有中立者勝利
      userentrys_r.foreach { userentry =>
        val role = userentry.get_role
        role match {
          case RoleAllie =>
            if (userentry.live.is)
              userentry.add_user_flag(UserEntryFlagEnum.VICTORY).save
          case RoleBryan =>
            if ((userentry.live.is) && (userentry.location.is == LocationEnum.ERSTWHILE_ALTER.toString))
              userentry.add_user_flag(UserEntryFlagEnum.VICTORY).save
          case RoleDaniel => 
            if ((userentry.live.is) && (victory == RoomVictoryEnum.HUNTER_WIN))
              userentry.add_user_flag(UserEntryFlagEnum.VICTORY).save
          case RoleCatherine =>
            val userentrys_live = userentrys_r.filter(_.live.is)
            if ((userentry.live.is) && (userentrys_live.length <= 2))
              userentry.add_user_flag(UserEntryFlagEnum.VICTORY).save
          case xs => ;
        }
      }
      
      var is_done = true
      do {
        is_done = true
        userentrys_r.foreach { userentry =>
          // Agnes 要最後
          val role = userentry.get_role
          if ((role == RoleAgnes) && (userentry.hasnt_user_flag(UserEntryFlagEnum.VICTORY))) {
            val index = userentrys_r.indexOf(userentry)
            val equiv_index = 
              if (!userentry.revealed.is)
                (index+1) % (userentrys_r.length)
              else
                (index + userentrys_r.length - 1) % (userentrys_r.length)
            
            val equiv_userentry = userentrys_r(equiv_index)
            //val equiv_role      = equiv_userentry.get_role
            if (check_user_victory(equiv_userentry, victory.toString)) {
              userentry.add_user_flag(UserEntryFlagEnum.VICTORY).save
              is_done = false
            } 
          } else if (((role == RoleCassandra) || (role == RoleCheshire))
                     && (userentry.hasnt_user_flag(UserEntryFlagEnum.VICTORY))) {
            val target_users = userentrys.filter(_.id.is == userentry.target_user.is)
            if (target_users.length != 0) {
              if (check_user_victory(target_users(0), victory.toString)) {
                userentry.add_user_flag(UserEntryFlagEnum.VICTORY).save
                is_done = false
              } 
            }
          }
        }
      } while(!is_done)
      
      process_victory(room, roomround, userentrys, victory)
      true
    } else
      false
  }
  
  def check_user_victory(userentry : UserEntry, victory_str : String) : Boolean = {
    val role = userentry.get_role
    
    if ((victory_str == RoomVictoryEnum.SHADOW_WIN.toString) && (role.role_side == RoleSideEnum.SHADOW)) 
      true
    else if ((victory_str == RoomVictoryEnum.HUNTER_WIN.toString) && (role.role_side == RoleSideEnum.HUNTER)) 
      true
    else if ((victory_str == RoomVictoryEnum.DUAL_WIN.toString) && (role.role_side != RoleSideEnum.NEUTRAL)) 
      true
    else if ((victory_str == RoomVictoryEnum.LOVER_WIN.toString) && (userentry.has_user_flag(UserEntryFlagEnum.LOVER)))
      true
    else if (userentry.has_user_flag(UserEntryFlagEnum.VICTORY))
      true
    else 
      false
  }
  
  def process_victory(room : Room, roomround: RoomRound, userentrys: List[UserEntry], victory : RoomVictoryEnum.Value) = {
    val new_roomround = RoomRound.create.room_id(room.id.is).round_no(roomround.round_no.is + 1)
                                        .last_round(roomround.id.is)
    new_roomround.save
    val talk = Talk.create.roomround_id(new_roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                   .message("遊戲結束 "+ (new java.util.Date).toString)
    talk.save
    
    val new_phase = RoomPhase.create.roomround_id(new_roomround.id.is).phase_no(0)
                                     .phase_type(RoomPhaseEnum.ENDED.toString)
    new_phase.save
    
    val userentrys_v = userentrys.filter(x => (!x.revoked.is) && (x.has_user_flag(UserEntryFlagEnum.VICTORY)))
    val userentrys_v_role = userentrys_v.map(_.get_real_role.role_enum.toString)
    
    room.status(RoomStatusEnum.ENDED.toString).victory(victory.toString).victory_all(userentrys_v_role.mkString(","))
    room.save

    RoomActor.sendRoomMessage(room.id.is, SessionVarSet(room = room, roomround = new_roomround, roomphase = new_phase, userentrys = userentrys))
    RoomActor.sendRoomMessage(room.id.is, RoomForceUpdate(room.id.is ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR)))
  }
  
  def next_player(room: Room, roomround: RoomRound, roomphase: RoomPhase, userentrys: List[UserEntry]) : Unit = {
    val userentrys_rl = userentrys.filter(x => (!x.revoked.is) && 
                                           (((x.live.is) && (x.hasnt_room_flag(UserEntryRoomFlagEnum.SKIPPED)))
                                            || (x.id.is == roomphase.player.is)
                                            || ((x.get_role == RoleUndead) && (x.hasnt_user_flag(UserEntryFlagEnum.SEALED)) && (x.hasnt_room_flag(UserEntryRoomFlagEnum.SUDDENDEATH)))
                                            ))
    val currentplayer = userentrys.filter(x => x.id.is == roomphase.player.is)(0)
    val old_player_index = userentrys_rl.map(_.id.is).indexOf(roomphase.player.is)
    var player_index = old_player_index
    var new_phase_no = roomphase.phase_no.is + 1
    var new_roomround = roomround
    var additional    = roomphase.additional.is

    var while_bound   = 0
    
    if ((additional <= 0) || (!currentplayer.live.is)) { // && (currentplayer.live.is)) {
      additional = 0
      do {
        while_bound = while_bound + 1
        player_index = player_index + 1
        if (player_index  >= userentrys_rl.length) {
          // 新回合
          player_index = 0
          new_phase_no = 0

          new_roomround = RoomRound.create.room_id(room.id.is).round_no(roomround.round_no.is + 1)
                                   .last_round(roomround.id.is)
          new_roomround.save

          val talk = Talk.create.roomround_id(new_roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                         .message("第 " + (new_roomround.round_no.is.toString) + " 日 "+ (new java.util.Date).toString)
          talk.save
        }
        
        val next_player1 = userentrys_rl(player_index)
        if ((!next_player1.live.is) && (next_player1.get_role == RoleUndead) &&
            (next_player1.hasnt_user_flag(UserEntryFlagEnum.SEALED))) {
          next_player1.lower_damage(1)
          if (next_player1.damaged.is < RoleUndead.life) {
            next_player1.damaged(8).live(true)
                        .add_user_flag(UserEntryFlagEnum.REVIVED)
                        .add_role_flag(UserEntryRoleFlagEnum.REVIVED_AURA)
          }
          next_player1.save
        }
        
      } while ((!userentrys_rl(player_index).live.is) && (while_bound < 100))
    } else
      additional = additional - 1
    
    val next_player = userentrys_rl(player_index)
    if ((player_index == old_player_index) && (!next_player.live.is)) {
      warn("warn : next_player dead same player")
      warn("room : " + room.id.is)
      warn("roomround : " + roomround.id.is)
      warn("roomphase : " + roomphase.id.is)
      //warn("currentuserentry : " + currentuserentry.id.is)
    } 

    if (while_bound >= 100) {
      warn("warn : while_bound >= 100")
      warn("room : " + room.id.is)
      warn("roomround : " + roomround.id.is)
      warn("roomphase : " + roomphase.id.is)
    }
    
    next_player.remove_role_flag(UserEntryRoleFlagEnum.ROLE_MOVESKILL_USED)
    if (next_player.get_role == RoleWerewolf)
      next_player.remove_role_flag(UserEntryRoleFlagEnum.AMBUSH)
    next_player.remove_user_flag(UserEntryFlagEnum.GUARDIAN)
    next_player.remove_user_flag(UserEntryFlagEnum.BARRIER)
    currentplayer.remove_user_flag(UserEntryFlagEnum.ADVENT)
    currentplayer.remove_user_flag(UserEntryFlagEnum.CHOCOLATE)
    currentplayer.remove_user_flag(UserEntryFlagEnum.DIABOLIC)
    currentplayer.remove_user_flag(UserEntryFlagEnum.FROG)
    currentplayer.remove_user_flag(UserEntryFlagEnum.TAUNT)
    if (currentplayer.get_role == RoleEmi)
      currentplayer.remove_role_flag(UserEntryRoleFlagEnum.ENHANCED)
    currentplayer.save
    
    if ((next_player.revealed) && (next_player.get_role == RoleCatherine) && (next_player.hasnt_user_flag(UserEntryFlagEnum.SEALED)))
      next_player.lower_damage(1)
    if ((next_player.revealed) && (next_player.get_role == RoleGinger) && (next_player.hasnt_user_flag(UserEntryFlagEnum.SEALED))) {
      val shadow_rl = userentrys_rl.filter(x => (x.live.is) && (x.get_role.role_side == RoleSideEnum.SHADOW)) // (x.revealed.is) && 
      val hunter_rl = userentrys_rl.filter(x => (x.live.is) && (x.get_role.role_side == RoleSideEnum.HUNTER)) // (x.revealed.is) && 
      if (shadow_rl.length > hunter_rl.length)
        next_player.lower_damage(2)
    }
    if (next_player.has_user_flag(UserEntryFlagEnum.POISON)) {
      next_player.inflict_damage(next_player.user_flags.is.count(_.toString == UserEntryFlagEnum.POISON.toString), 
                                 next_player)
    }
    next_player.save
    
    val deadline =
      if (next_player.has_room_flag(UserEntryRoomFlagEnum.AUTOVOTED))
        math.max(15, room.move_time.is * 3 / 4)
      else
        room.move_time.is
    
    val new_phase = RoomPhase.create.roomround_id(new_roomround.id.is).phase_no(new_phase_no).additional(additional)
                             .phase_type(RoomPhaseEnum.MOVEMENT.toString).player(next_player.id.is)
                             .deadline(PlummUtil.dateAddSecond(new java.util.Date(), deadline))
    new_phase.save
    
    GameProcessor.check_death(next_player, next_player, Action.create.roomround_id(new_roomround.id.is), userentrys)
    if (!GameProcessor.check_victory(room, new_roomround, userentrys)) {
      
      if (!next_player.live.is)  {
        GameProcessor.next_player(room, new_roomround, new_phase, userentrys) 
      } else {
        RoomActor.sendRoomMessage(room.id.is, SessionVarSet(room = room, roomround = new_roomround, roomphase = new_phase, userentrys = userentrys))
        RoomActor.sendRoomMessage(room.id.is, RoomForceUpdate(room.id.is ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR)))
      }
    }
  }
  
  def flip(actioner : UserEntry, action : Action, userentrys : List[UserEntry]) : Unit = {
    val action1 = Action.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is)
                               .mtype(MTypeEnum.ACTION_FLIP.toString)
    action1.save
    val talk = Talk.create.roomround_id(action.roomround_id.is).actioner_id(actioner.id.is)
                               .mtype(MTypeEnum.ACTION_FLIP.toString)
    talk.save
    talk.send(actioner.room_id.is)
    
    val role = actioner.get_role
    if ((role.role_side == RoleSideEnum.SHADOW) && 
       (userentrys.filter(x => (x.get_role.role_side == RoleSideEnum.SHADOW) && (x.revealed.is)).length == 0)) {
      val live_evans = userentrys.filter(x => (x.live.is) && (x.get_role == RoleEvan))
      if (live_evans.length != 0) {
        val live_evan = live_evans(0)
        live_evan.add_user_flag(UserEntryFlagEnum.LOVER)
        live_evan.target_user(actioner.id.is)
        actioner.add_user_flag(UserEntryFlagEnum.LOVER)
        
        //println("Before Find")
        Room.find(By(Room.id, actioner.room_id.is)) match {
          case Full(room) if (room.has_flag(RoomFlagEnum.EVAN_HEAL)) =>
            if (actioner.live.is) {
              //println("Lover Flip : Actioner Healed " + actioner.handle_name.is)
              actioner.damaged(0)
            }
            live_evan.damaged(0)
          case xs => error("No Room Found (Lover Flip)");
        }
        //println("After Find")
        
        if (!live_evan.revealed.is)
          flip(live_evan, action, userentrys)
        else
          live_evan.save
      }
    }
    
    actioner.revealed(true).save
    
    val live_unrevealed = userentrys.filter(x =>(!x.revoked.is) && (x.get_role != RoleDetective) && (x.live.is) && (!x.revealed.is))
    if (live_unrevealed.length == 0) {
      val live_detectives = userentrys.filter(x =>(x.get_role == RoleDetective) && (x.live.is))
      live_detectives.foreach { live_detective =>
        val saved_damaged = live_detective.damaged.is
        live_detective.damaged(99)
        GameProcessor.check_death(live_detective, live_detective, action, userentrys)
        live_detective.damaged(saved_damaged).save
      }
    }
    
    actioner
  }
  
  def attack(actioner : UserEntry, actionee : UserEntry, action : Action, userentrys : List[UserEntry], is_ambush : Boolean = false) = {
    val actioner_role = actioner.get_role
    
    val random1d6 = GameProcessor.random.nextInt(6) + 1
    val random1d4 = GameProcessor.random.nextInt(4) + 1
        
    var attack_power = 0
    var attack_str   = ""
        
    if (actionee.has_user_flag(UserEntryFlagEnum.BARRIER)) {
      attack_power = 0
      attack_str   = "防護罩防禦住攻擊"
    } else if (actionee.has_user_flag(UserEntryFlagEnum.GUARDIAN)) {
      attack_power = 0
      attack_str   = "守護天使防禦住攻擊"
    } else if (actioner.has_user_flag(UserEntryFlagEnum.MISSED) && actioner.has_item(CardEnum.B_DAGGER)) {
      if ((actioner.revealed) && (actioner.get_role.role_side == RoleSideEnum.HUNTER)) 
        attack_power = 6
      else 
        attack_power = 5
      attack_str = ("攻擊力：" + attack_power)
    } else if (is_ambush) {
      attack_power = 5
      attack_str = ("攻擊力：" + attack_power)
    } else if (((actioner.revealed.is) && (actioner.get_role == RoleValkyrie) 
               && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED))) ||
              (actioner.has_item(CardEnum.B_MASAMUNE))) {
      Room.find(By(Room.id, actioner.room_id.is)) match {
        case Full(room) if (room.has_flag(RoomFlagEnum.VALKYRIE_ENHANCE) &&
                            (actioner.revealed.is) && (actioner.get_role == RoleValkyrie)) =>
          attack_power = random1d6
          attack_str = ("1D6=" + random1d6 + " 攻擊力：" + attack_power)
        case xs =>  
          attack_power = random1d4
          attack_str = ("1D4=" + random1d4 + " 攻擊力：" + attack_power)
      }  
    } else {
      attack_power = math.abs(random1d6 - random1d4)
      attack_str = ("1D6=" + random1d6 + ",1D4=" + random1d4 + " 攻擊力：" + attack_power)
    }
        
    var is_append = false
    if (actioner.has_item(CardEnum.W_HOLY_ROBE) || actionee.has_item(CardEnum.W_HOLY_ROBE)) {
      attack_power = math.max(0, attack_power - 1)
      attack_str += "-1(袍)"
      is_append = true
    }
    
    if ((actionee.get_role.role_side == RoleSideEnum.SHADOW) &&
        (actionee.revealed)){
      val reviveds = userentrys.filter(x => (x.live.is) &&
        (x.get_role == RoleUndead) &&
        (x.has_role_flag(UserEntryRoleFlagEnum.REVIVED_AURA)))
      if (reviveds.length != 0) {
        attack_power = math.max(0, attack_power - 1)
        attack_str += "-1(闇)"
        is_append = true
      }
    }
    
    
    if ((attack_power > 0) && (actioner.has_item(CardEnum.B_CHAINSAW))) {
      attack_power += 1
      attack_str += "+1(鋸)"
      is_append = true
    }
    if ((attack_power > 0) && (actioner.has_item(CardEnum.B_RUSTED_BROAD_AXE))) {
      attack_power += 1
      attack_str += "+1(斧)"
      is_append = true
    }
    if ((attack_power > 0) && (actioner.has_item(CardEnum.B_BUTCHER_KNIFE))) {
      attack_power += 1
      attack_str += "+1(刀)"
      is_append = true
    }
    if (((actioner.revealed.is) && (actioner.get_role == RoleValkyrie) 
          && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED))) &&
         (actioner.has_item(CardEnum.B_MASAMUNE))) {
      attack_power += 1
      attack_str += "+1(妖)"
      is_append = true
    }
    if ((attack_power > 0) && (actioner.has_item(CardEnum.W_LANCE_OF_LONGINUS)) && (actioner.get_role.role_side == RoleSideEnum.HUNTER) && (actioner.revealed.is)) {
      attack_power += 2
      attack_str += "+2(槍)"
      is_append = true
    }
    //if ((attack_power > 0) && (actionee.location.is == LocationEnum.WIERD_WOODS.toString) &&
    //    (actioner.get_role == RoleWitch) && (actioner.revealed.is) &&
    //    (!actioner.has_user_flag(UserEntryFlagEnum.SEALED))) 
    //  actionee.add_user_flag(UserEntryFlagEnum.FROG)
    
    
    if ((attack_power > 0) && (actioner.revealed.is) && (actioner != actionee) &&
        (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED))) {
      if ((actioner.get_role == RoleFeng) && (actioner.items.length == 0)){
        if (actionee.items.length <= 1) {
          attack_power += 2
          attack_str += "+2(格鬥)"
        } else {
          attack_power += 1
          attack_str += "+1(格鬥)"
        }
        is_append = true
      } else if ((actioner.get_role == RoleEmi) && (actioner.has_role_flag(UserEntryRoleFlagEnum.ENHANCED))){
        attack_power += 1
        attack_str += "+1(艾米)"
        is_append = true
      } else if (actioner.get_role == RoleWicked) {
        if ((actionee.location.is == LocationEnum.GRAVEYARD.toString) || 
            (actionee.location.is == LocationEnum.WIERD_WOODS.toString)) {
          attack_power += 1
          attack_str += "+1(邪惡)"
          is_append = true
        }
        if (((actionee.get_role.role_side == RoleSideEnum.HUNTER) ||
            (actionee.get_role.role_side == RoleSideEnum.NEUTRAL)) &&
            (actionee.revealed.is)) {
          attack_power += 2
          attack_str += "+2(邪惡)"
          is_append = true
        }
      } else if ((attack_power > 1) && (actioner.get_role == RoleBellandona) && 
                 (actionee.revealed.is)) {
      
        Room.find(By(Room.id, actioner.room_id.is)) match {
          case Full(room) =>
            if (room.has_flag(RoomFlagEnum.BELLANDONA_CHOOSE)) {
              actioner.role(actionee.role.is.substring(0,2) + actioner.role.is)
              actioner.target_user(actionee.target_user.is)
              //actioner_role = actionee.get_role
      
              val actionee_role = actionee.get_role
              if (actioner.damaged.is >= actionee_role.role_life)
                actioner.damaged(actionee_role.role_life - 1)
      
              actioner.save
              //GameProcessor.check_death(actioner, actioner, action, userentrys)
              //actioner.save
            }
          
          case xs => ;
        }
      } else if ((actioner.get_role == RoleViper)) { //&& 
                 // (actionee.hasnt_user_flag(UserEntryFlagEnum.POISON))) {
        actionee.add_user_flag(UserEntryFlagEnum.POISON)
      }
    }
    
    
    if (is_append) {
      attack_str += "=" + attack_power
    }  
        
    // 吸血鬼回復 HP
    if ((actioner.revealed.is) && (actioner.get_role == RoleVampire) && (attack_power != 0) &&
        (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED))) {
      Room.find(By(Room.id, actioner.room_id.is)) match {
        case Full(room) if (room.has_flag(RoomFlagEnum.VAMPIRE_WEAKEN) &&
          actioner.has_item(CardEnum.B_MACHINE_GUN)) => 
          actioner.lower_damage(1)
          attack_str += "，並回復 1 點損傷"
        case xs =>  
          actioner.lower_damage(2)
          attack_str += "，並回復 2 點損傷"
      }
      actioner.save
    }
    
    // 鮑伯搶奪
    if (((actioner_role == RoleBob) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED))) &&
       // (actioner.has_item(CardEnum.W_SILVER_ROSARY)) && 
       (attack_power != 0) ) {
      if (actionee.items.length > 0) {
        val robbed_item = rob_single(actioner, actionee)
      
        attack_str += "，並搶奪 " + robbed_item.card_name
      }
      actioner.save
    }
    
    // 物攻嘲諷
    /*
    if (((actioner_role == RoleADecoy) && (actioner.revealed.is) && (actioner.hasnt_user_flag(UserEntryFlagEnum.SEALED))) &&
       // (actioner.has_item(CardEnum.W_SILVER_ROSARY)) && 
       (attack_power == 0) ) {
      actionee.add_user_flag(UserEntryFlagEnum.TAUNT)
    } */
   
    if ((attack_power == 0) && (actioner.hasnt_user_flag(UserEntryFlagEnum.MISSED))) {
      actioner.add_user_flag(UserEntryFlagEnum.MISSED).save
    } else if (attack_power > 0) {
      actioner.remove_user_flag(UserEntryFlagEnum.MISSED).save
    }
        
    (attack_power, attack_str)
  }
  
  //def next_phase = {
  //  
  //}
  
  def abandon(room : Room) = {
    val roomround = RoomRound.find(By(RoomRound.room_id, room.id.is), OrderBy(RoomRound.round_no, Descending)).get
    val new_roomround = RoomRound.create.room_id(room.id.is).round_no(roomround.round_no.is + 1)
                                        .last_round(roomround.id.is)
    new_roomround.save
    val talk = Talk.create.roomround_id(new_roomround.id.is).mtype(MTypeEnum.MESSAGE_GENERAL.toString)
                   .message("遊戲結束 "+ (new java.util.Date).toString)
    talk.save
     
    val new_phase = RoomPhase.create.roomround_id(new_roomround.id.is).phase_no(0)
                            .phase_type(RoomPhaseEnum.ENDED.toString)
    new_phase.save
             
    room.status(RoomStatusEnum.ENDED.toString).victory(RoomVictoryEnum.ABANDONED.toString)
    room.save
            
    RoomActor ! SessionVarSet(room = room, roomround = new_roomround, roomphase = new_phase)
    //RoomActor.sendRoomMessage(room_id, RoomForceUpdate(room_id ,List(ForceUpdateEnum.USER_TABLE, ForceUpdateEnum.TALK_TABLE, ForceUpdateEnum.TIME_TABLE, ForceUpdateEnum.ACTION_BAR)))
    RoomActor.sendRoomMessage(room.id.is, RoomForceOut(room.id.is))
  }
}
