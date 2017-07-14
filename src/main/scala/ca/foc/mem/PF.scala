package ca.foc.mem

import scalaz.stream.Process
import scalaz.stream.io
import scalaz.concurrent.Task
import scalaz.\/
import scalaz.-\/
import scalaz.\/-
import ca.foc.mem.PokerFormulas._
import scala.io.StdIn
import scala.collection.immutable.SortedMap
import scalaz.effect.IO
import scalaz.effect.Effect
import scalaz.syntax.traverse._
import scalaz.std.list._
import scalaz.Id
import scalaz.syntax.std.string._
import scalaz.syntax.std.StringOps
import scalaz.syntax.std.StringOps._

object PF extends App {
  private def printOptions(options: Map[Int, String]) = {
    for ((idx, option) <- options.toList.sortBy(_._1))
      yield IO.putStrLn(idx + ") " + option)
  }.sequence.void
  private def formatDeal(d: DealInfo) = d.cut + "." + d.seconds.map {
    case 10 => "A"
    case n  => n.toString
  }.mkString
  def inputInt: IO[Int] = IO.readLn.flatMap {
    _.parseInt.map(IO(_)).getOrElse(IO.putStrLn("Invalid number. Try again.").flatMap(_ => inputInt))
  }
  val selectOption = Menu.selectOption(IO.putStrLn, printOptions, inputInt)_
  val findDeal = Menu.selectDeal(selectOption).flatMap(d => IO.putStrLn(formatDeal(d)))

  findDeal.unsafePerformIO()
}

object Menu {
  def selectOption(displayPrompt: String => IO[Unit],
                   displayOptions: Map[Int, String] => IO[Unit],
                   inputSelection: IO[Int])(prompt: String, options: Map[Int, String]): IO[Int] =
    {
      for (
        _ <- displayPrompt(prompt);
        _ <- displayOptions(options);
        option <- inputSelection
      ) yield option
    }.flatMap { opt =>
      if (options.contains(opt)) IO(opt)
      else for (
        _ <- displayPrompt("Invalid Option. Try again.");
        option <- selectOption(displayPrompt, displayOptions, inputSelection)(prompt, options)
      ) yield option
    }
  def selectDeal(selectOption: (String, Map[Int, String]) => IO[Int]) =
    BuildHand.build(selectOption).flatMap { hand =>
      val deals = ((for (players <- 3 to 10) yield findHand(MemDeck, hand, players).map(players -> _)).flatten).toMap
      selectOption("Number of Players?", deals.map { case (p, _) => p -> (p + " Players") }).map(deals(_))
    }
}
sealed trait HandBuilder {
  val prompt: String
  def options: Map[Int, String]
  def select(opt: Int): (HandBuilder \/ Hand)
  protected val suitMap = Map(1 -> S, 2 -> H, 3 -> C, 4 -> D);
  protected val valueMap = VALUES.map(v => v.intVal -> v).toMap
  def build(selectOption: (String, Map[Int, String]) => IO[Int]): IO[Hand] = {
    def loop(b: HandBuilder): IO[Hand] =
      for (
        option <- selectOption(b.prompt, b.options);
        hand <- b.select(option).leftMap(loop).map(IO(_)).merge
      ) yield hand
    loop(this)
  }
}
object BuildHand extends HandBuilder {
  private val builders =
    Map(1 -> StraightBuilder, 2 -> FlushBuilder, 3 -> FullHouseBuilder, 4 -> QuadsBuilder, 5 -> StraightFlushBuilder)
  val prompt = "Please select a hand."
  def options = builders.mapValues(_.toString)
  def select(opt: Int) = -\/(builders(opt))
}
object StraightBuilder extends HandBuilder {
  override def toString = "Straight"
  val prompt = "High Card?"
  def options = valueMap.filter { case (_, card) => card == A || card.intVal >= 5 }.mapValues(_.toString)
  def select(opt: Int) = \/-(Straight(valueMap(opt)))
}
object FlushBuilder extends HandBuilder {
  override def toString = "Flush"
  val prompt = "Suit?"
  def options = suitMap.mapValues(_.toString)
  def select(opt: Int): (HandBuilder \/ Hand) = -\/(new SuitedFlushBuilder(suitMap(opt)))
}
private final class SuitedFlushBuilder(suit: Suit) extends HandBuilder {
  val prompt = "High Card?"
  def options = valueMap.filter { case (_, card) => card == A || card.intVal >= 6 }.mapValues(_.toString)
  def select(opt: Int) = \/-(Flush(suit, valueMap(opt)))
}
object FullHouseBuilder extends HandBuilder {
  override def toString = "Full House"
  val prompt = "First Value?"
  def options = valueMap.mapValues(_.toString)
  def select(opt: Int): (HandBuilder \/ Hand) = -\/(new FullHouseBuilder2(valueMap(opt)))
}
private final class FullHouseBuilder2(v1: CardVal) extends HandBuilder {
  val prompt = "Second Value?"
  def options = valueMap.filter(_._2 != v1).mapValues(_.toString)
  def select(opt: Int) = \/-(FullHouse(v1, valueMap(opt)))
}
object QuadsBuilder extends HandBuilder {
  override def toString = "Four of a Kind"
  val prompt = "Value?"
  def options = valueMap.mapValues(_.toString)
  def select(opt: Int) = \/-(Quads(valueMap(opt)))
}
object StraightFlushBuilder extends HandBuilder {
  override def toString = "Straight Flush"
  val prompt = "Suit?"
  def options = suitMap.mapValues(_.toString)
  def select(opt: Int): (HandBuilder \/ Hand) = -\/(new SuitedStraightFlushBuilder(suitMap(opt)))
}
private final class SuitedStraightFlushBuilder(suit: Suit) extends HandBuilder {
  val prompt = "High Card?"
  def options = valueMap.filter { case (_, card) => card == A || card.intVal >= 5 }.mapValues(_.toString)
  def select(opt: Int) = \/-(StraightFlush(suit, valueMap(opt)))
}