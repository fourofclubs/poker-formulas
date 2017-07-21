package ca.foc.pf

import ca.foc.pf.PokerFormulas.{ DealInfo, Flush, FullHouse, Hand, Quads, Straight, StraightFlush, findHand }
import scalaz.{ -\/ => -\/, \/ => \/, \/- => \/- }
import scalaz.effect.IO
import scalaz.std.list.listInstance
import scalaz.syntax.std.string.ToStringOpsFromString
import scalaz.syntax.traverse.{ ToFunctorOps, ToTraverseOps }
import ca.foc.pf.PokerFormulas.Trips

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
    Map(1 -> TwoPairBuilder, 2 -> TripsBuilder, 3 -> StraightBuilder, 4 -> FlushBuilder,
      5 -> FullHouseBuilder, 6 -> QuadsBuilder, 7 -> StraightFlushBuilder)
  val prompt = "Please select a hand."
  def options = builders.mapValues(_.toString)
  def select(opt: Int) = -\/(builders(opt))
}
object TwoPairBuilder extends HandBuilder {
  override def toString = "Two Pair"
  val prompt = "First Value?"
  def options = valueMap.mapValues(_.toString)
  def select(opt: Int): (HandBuilder \/ Hand) = -\/(new TwoPairBuilder2(valueMap(opt)))
}
private final class TwoPairBuilder2(v1: CardVal) extends HandBuilder {
  val prompt = "Second Value?"
  def options = valueMap.filter(_._2 != v1).mapValues(_.toString)
  def select(opt: Int): (HandBuilder \/ Hand) = -\/(new TwoPairBuilder2(valueMap(opt)))
}
object TripsBuilder extends HandBuilder {
  override def toString = "Three of a Kind"
  val prompt = "Value?"
  def options = valueMap.mapValues(_.toString)
  def select(opt: Int) = \/-(Trips(valueMap(opt)))
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