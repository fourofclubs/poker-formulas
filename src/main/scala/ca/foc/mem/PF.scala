package ca.foc.mem

import scalaz.stream.Process
import scalaz.stream.io
import scalaz.concurrent.Task
import scalaz.\/
import scalaz.-\/
import scalaz.\/-
import ca.foc.mem.PokerFormulas.Hand
import ca.foc.mem.PokerFormulas.Straight
import ca.foc.mem.PokerFormulas.Flush
import ca.foc.mem.PokerFormulas.FullHouse
import ca.foc.mem.PokerFormulas.Quads
import ca.foc.mem.PokerFormulas.StraightFlush

object PF extends App {
  val in = io.stdInLines.once
  val inR = in.runLog.run
  val p: Process[Task, Int] = Process.range(0, 10)
  val mapResult = p.map(x => "the value is " + x).runLog.run
  val filterResult = p.filter(x => x % 2 == 0).runLog.run
  val p2 = p.flatMap { x => Process(x, x + 1, x + 2) }.runLog.run

  println(inR)
  println(mapResult)
  println(filterResult)
  println(p2)
}

sealed trait HandBuilder {
  def options: Map[Int, String]
  def select(opt: Int): (HandBuilder \/ Hand)
  protected val suitMap = Map(1 -> S, 2 -> H, 3 -> C, 4 -> D);
  protected val valueMap = VALUES.map(v => v.intVal -> v).toMap
}

final class BuildHand extends HandBuilder {
  private val builders =
    Map(1 -> StraightBuilder, 2 -> FlushBuilder, 3 -> FullHouseBuilder, 4 -> QuadsBuilder, 5 -> StraightFlushBuilder)
  def options = builders.mapValues(_.toString)
  def select(opt: Int) = -\/(builders(opt))
}
object StraightBuilder extends HandBuilder {
  override def toString = "Straight"
  def options = valueMap.mapValues(_.toString)
  def select(opt: Int) = \/-(Straight(valueMap(opt)))
}
object FlushBuilder extends HandBuilder {
  override def toString = "Flush"
  def options = suitMap.mapValues(_.toString)
  def select(opt: Int): (HandBuilder \/ Hand) = -\/(new SuitedFlushBuilder(suitMap(opt)))
}
private final class SuitedFlushBuilder(suit: Suit) extends HandBuilder {
  def options = VALUES.map(v => v.intVal -> v.toString).toMap
  def select(opt: Int) = \/-(Flush(suit, valueMap(opt)))
}
object FullHouseBuilder extends HandBuilder {
  override def toString = "Full House"
  def options = valueMap.mapValues(_.toString)
  def select(opt: Int): (HandBuilder \/ Hand) = -\/(new FullHouseBuilder2(valueMap(opt)))
}
private final class FullHouseBuilder2(v1: CardVal) extends HandBuilder {
  def options = valueMap.mapValues(_.toString)
  def select(opt: Int) = \/-(FullHouse(v1, valueMap(opt)))
}
object QuadsBuilder extends HandBuilder {
  override def toString = "Four of a Kind"
  def options = valueMap.mapValues(_.toString)
  def select(opt: Int) = \/-(Quads(valueMap(opt)))
}
object StraightFlushBuilder extends HandBuilder {
  override def toString = "Straight Flush"
  def options = suitMap.mapValues(_.toString)
  def select(opt: Int): (HandBuilder \/ Hand) = -\/(new SuitedStraightFlushBuilder(suitMap(opt)))
}
private final class SuitedStraightFlushBuilder(suit: Suit) extends HandBuilder {
  def options = valueMap.mapValues(_.toString)
  def select(opt: Int) = \/-(StraightFlush(suit, valueMap(opt)))
}