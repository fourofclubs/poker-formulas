package ca.foc.mem

import java.io.{ File, PrintStream }

import scala.annotation.migration

object PokerFormulas extends App {
  def find[C >: Card](d: Deck, ps: List[C ⇒ Boolean], players: Int): List[List[Int]] =
    if (d.size < players) List()
    else ps match {
      case List() ⇒ List(List())
      case p :: ps2 ⇒ {
        val s1 = if (p(d.cardAt(players))) find(d.drop(players), ps2, players).map(0 :: _) else List()
        val s2 =
          if (d.size > players && p(d.cardAt(players + 1)))
            find(Deck(d.cardAt(players) :: d.drop(players + 1).cards), ps2, players).map(players :: _)
          else List()
        val s3 = {
          val index = d.cards.take(players).indexWhere(p)
          if (index >= 0) find(d.drop(players), ps2, players).map(index + 1 :: _)
          else List()
        }
        s1 ++ s2 ++ s3
      }
    }

  case class Predicate[-A](p: A => Boolean) extends (A => Boolean) {
    def apply(a: A) = p(a)
    def &&[B <: A](p2: B => Boolean) = Predicate[B](a => p(a) && p2(a))
    def ||[B <: A](p2: B => Boolean) = Predicate[B](a => p(a) || p2(a))
    def unary_! = Predicate[A](a => !p(a))
  }
  implicit def functionToPredicate[A](p: A => Boolean): Predicate[A] = Predicate(p)

  val any = (c: Card) ⇒ true
  def isValue(v: CardVal) = (c: Card) ⇒ c.value == v
  def isSuit(s: Suit) = (c: Card) => c.suit == s
  def isCard(c: Card) = (c2: Card) => c2 == c
  def isCard(v: CardVal, s: Suit): Card => Boolean = isCard(Card(v, s))
  def isLower(v: CardVal) = (c: Card) => c.value.intVal < v.intVal && c.value != A
  def inRange(n1: Int, n2: Int) = (c: Card) => (n1 to n2).contains(c.value.intVal)

  sealed trait Hand {
    def searchers: Iterable[List[Card => Boolean]]
    def verify(cards: List[Card]): Boolean
  }
  case class TwoPair(v1: CardVal, v2: CardVal) extends Hand {
    override def toString = s"$v1+$v2"
    override def equals(o: Any) = if (o.isInstanceOf[TwoPair]) {
      val that = o.asInstanceOf[TwoPair]
      Set(that.v1, that.v2) == Set(v1, v2)
    } else false
    val searchers = List(isValue(v1), isValue(v1), isValue(v2), isValue(v2), !isValue(v1) && !isValue(v2)).permutations.toSet
    def verify(cards: List[Card]) = cards.count(_.value == v1) == 2 && cards.count(_.value == v2) == 2
  }
  case class Trips(v: CardVal) extends Hand {
    override def toString = s"T$v"
    val searchers = List(isValue(v), isValue(v), isValue(v), !isValue(v), !isValue(v)).permutations.toSet
    def verify(cards: List[Card]) = cards.count(_.value == v) == 3 && cards.groupBy(_.value).size == 3
  }
  case class Straight(highVal: CardVal) extends Hand {
    override def toString = s"S($highVal)"
    val searchers = List(isValue(highVal), isValue(highVal - 1), isValue(highVal - 2),
      isValue(highVal - 3), isValue(highVal - 4)).permutations.toSet
    def verify(cards: List[Card]) = {
      val values = cards.groupBy(_.value).keys.toSet
      values.contains(highVal) && values.contains(highVal - 1) && values.contains(highVal - 2) &&
        values.contains(highVal - 3) && values.contains(highVal - 4)
    }
  }
  case class Flush(s: Suit, highVal: CardVal) extends Hand {
    override def toString = s"F($s,$highVal)"
    val searchers = (isCard(highVal, s) :: List.fill(4)(isSuit(s) && isLower(highVal))).permutations.toSet
    def verify(cards: List[Card]) = cards.forall(isSuit(s)) &&
      cards.maxBy(c => if (c.value == A) 14 else c.value.intVal).value == highVal
  }
  case class FullHouse(v1: CardVal, v2: CardVal) extends Hand {
    override def toString = s"$v1/$v2"
    val searchers = List(isValue(v1), isValue(v1), isValue(v1), isValue(v2), isValue(v2)).permutations.toSet
    def verify(cards: List[Card]) = cards.count(_.value == v1) == 3 && cards.count(_.value == v2) == 2
  }
  case class StraightFlush(s: Suit, highVal: CardVal) extends Hand {
    override def toString = s"SF($s,$highVal)"
    val searchers = List.fill(5)(isSuit(s) && inRange(highVal.intVal - 4, highVal.intVal)).permutations.toSet
    def verify(cards: List[Card]) = cards.contains((highVal, s)) && cards.contains((highVal - 1, s)) &&
      cards.contains((highVal - 2, s)) && cards.contains((highVal - 3, s)) && cards.contains((highVal - 4, s))
  }
  case class RoyalFlush(s: Suit) extends Hand {
    override def toString = s"RF($s)"
    val searchers = List(isCard(A, s), isCard(K, s), isCard(Q, s), isCard(J, s), isCard(v10, s)).permutations.toSet
    def verify(cards: List[Card]) = cards.contains((A, s)) && cards.contains((K, s)) && cards.contains((Q, s)) &&
      cards.contains((J, s)) && cards.contains((v10, s))
  }
  val HANDS: Set[Hand] = {
    (for (v1 <- VALUES; v2 <- VALUES; if (v1 != v2)) yield TwoPair(v1, v2)) ++
      (for (v <- VALUES) yield Trips(v)) ++
      (for (v <- VALUES; if v.intVal >= 5 || v == A) yield Straight(v)) ++
      (for (v1 <- VALUES; v2 <- VALUES; if (v1 != v2)) yield FullHouse(v1, v2)) ++
      (for (s <- SUITS; v <- VALUES; if (v.intVal >= 5)) yield StraightFlush(s, v)) ++
      (for (s <- SUITS) yield RoyalFlush(s))
  }

  def evaluate(players: Int)(d: DealInfo) = (d.seconds.sum + (d.seconds.count(_ == 0) * players * 2))
  case class HandInfo(hand: Hand, players: Int)
  case class DealInfo(cut: Int, seconds: List[Int])
  val seconds = {
    for (
      cut ← (0 to 51).par;
      d ← List(MemDeck.cut(cut));
      h ← HANDS.par;
      s <- h.searchers;
      players ← 3 to 10
    ) yield HandInfo(h, players) -> find(d, s, players).map(s => DealInfo(cut, s))
  }.toList.filter(!_._2.isEmpty).map(p => p._1 -> p._2.maxBy(evaluate(p._1.players)_)).toMap

  def fmt(h: HandInfo)(d: DealInfo) = s"${h.hand.toString}, ${h.players} : ${d.cut}-${d.seconds.mkString(",")}"

  val fh = new File("pokerFormulas-FH.csv")
  fh.delete()
  val pfh = new PrintStream(fh)

  val sortedValues = VALUES.toList.sortBy(v => if (v == A) 14 else v.intVal).reverse
  val t = for (v1 <- sortedValues) yield {
    for (p <- 3 to 10) yield {
      for (
        v2 <- sortedValues.filterNot(_ == v1)
      ) yield {
        val h = HandInfo(FullHouse(v1, v2), p)
        seconds.get(h).map(fmt(h)_).getOrElse("")
      }
    }.mkString("\t")
  }.mkString("\n")
  println(t.mkString("\n\n"))

  //  for ((h, d) <- seconds.toList.sortBy(_._1.players).sortBy(_._1.hand.toString))
  //    ps.println(h.hand.toString + ", " + h.players + ": " + d.cut + "-" + d.seconds.mkString(","))
}
