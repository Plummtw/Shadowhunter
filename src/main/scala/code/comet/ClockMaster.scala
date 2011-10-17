package org.plummtw.shadowhunter.comet

/*
case class SubscribeClock(clock : Clock)
case class UnsubClock(clock : Clock)
​
object ClockMaster extends Actor {
  private var clocks : List[Clock] = Nil
  def act = loop {
    react {
       case SubscribeClock(clk) =>
        clocks ::= clk
      case UnsubClock(clk) =>
        clocks -= clk
      case Tick =>
        clocks.foreach(_ ! Tick)
    }
  }
}
*/