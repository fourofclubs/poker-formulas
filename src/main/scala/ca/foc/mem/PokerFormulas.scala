package ca.foc.mem

import scala.language.implicitConversions

object PokerFormulas {
  def findHand(d: Deck, h: Hand, players: Int): Option[DealInfo] = {
    def evaluate(players: Int, d: DealInfo): Int = (d.seconds.sum + (d.seconds.count(_ == 0) * players * 3))
    def find[C >: Card](d: Deck, ps: List[C ⇒ Boolean], players: Int): List[List[Int]] = ps match {
      case List() ⇒ List(List())
      case p :: ps2 ⇒
        if (d.size < players) List() else {
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
    val deals = {
      for (
        cut ← (0 to 51);
        ps ← h.cards.permutations
      ) yield (find(d.cut(cut), ps, players)).map(DealInfo(cut, _))
    }.flatten
    if (deals.isEmpty) None else Some(deals.maxBy(evaluate(players, _)))
  }

  val any = (c: Card) ⇒ true
  def isValue(v: CardVal) = (c: Card) ⇒ c.value == v
  def isSuit(s: Suit) = (c: Card) ⇒ c.suit == s
  def isCard(c: Card) = (c2: Card) ⇒ c2 == c
  def isCard(v: CardVal, s: Suit): Card ⇒ Boolean = isCard(Card(v, s))
  def isLower(v: CardVal) = (c: Card) ⇒ (c.value.intVal < v.intVal || (v == A && c.value != A)) && c.value != A
  def inRange(n1: Int, n2: Int) = (c: Card) ⇒ (n1 to n2).contains(c.value.intVal)
  def pair(v: CardVal) = List.fill(2)(isValue(v))
  def trips(v: CardVal) = List.fill(3)(isValue(v))

  sealed trait Hand {
    def cards: List[Card ⇒ Boolean]
    def verify(cards: List[Card]): Boolean
  }
  case class TwoPair(v1: CardVal, v2: CardVal) extends Hand {
    override def toString = s"$v1+$v2"
    val cards = pair(v1) ++ pair(v2) :+ (!isValue(v1) && !isValue(v2))
    def verify(cards: List[Card]) = cards.count(_.value == v1) == 2 && cards.count(_.value == v2) == 2
  }
  case class Trips(v: CardVal) extends Hand {
    override def toString = s"T$v"
    val cards = trips(v) ++ List.fill(2)(!isValue(v))
    def verify(cards: List[Card]) = cards.count(_.value == v) == 3 && cards.groupBy(_.value).size == 3
  }
  case class Straight(highVal: CardVal) extends Hand {
    override def toString = s"S($highVal)"
    val cards = List.range(0, 4).map(n => isValue(highVal - n))
    def verify(cards: List[Card]) = {
      val values = cards.groupBy(_.value).keys.toSet
      values.contains(highVal) && values.contains(highVal - 1) && values.contains(highVal - 2) &&
        values.contains(highVal - 3) && values.contains(highVal - 4)
    }
  }
  case class Flush(s: Suit, highVal: CardVal) extends Hand {
    override def toString = s"F($s,$highVal)"
    val cards = (isCard(highVal, s) :: List.fill(4)(isSuit(s) && isLower(highVal)))
    def verify(cards: List[Card]) = cards.forall(isSuit(s)) &&
      cards.maxBy(c ⇒ if (c.value == A) 14 else c.value.intVal).value == highVal
  }
  case class FullHouse(v1: CardVal, v2: CardVal) extends Hand {
    override def toString = s"$v1/$v2"
    val cards = trips(v1) ++ pair(v2)
    def verify(cards: List[Card]) = cards.count(_.value == v1) == 3 && cards.count(_.value == v2) == 2
  }
  case class Quads(v: CardVal) extends Hand {
    override def toString = s"Q$v"
    val cards = (any :: List.fill(4)(isValue(v)))
    def verify(cards: List[Card]) = cards.count(_.value == v) == 4
  }
  case class StraightFlush(s: Suit, highVal: CardVal) extends Hand {
    override def toString = s"SF($s,$highVal)"
    val cards =
      if (highVal == A) isCard(A, s) +: List.fill(4)(inRange(10, 13))
      else List.fill(5)(isSuit(s) && inRange(highVal.intVal - 4, highVal.intVal))
    def verify(cards: List[Card]) = cards.contains((highVal, s)) && cards.contains((highVal - 1, s)) &&
      cards.contains((highVal - 2, s)) && cards.contains((highVal - 3, s)) && cards.contains((highVal - 4, s))
  }
  case class RoyalFlush(s: Suit) extends Hand {
    override def toString = s"RF($s)"
    val cards = StraightFlush(s, A).cards
    def verify(cards: List[Card]) = cards.contains((A, s)) && cards.contains((K, s)) && cards.contains((Q, s)) &&
      cards.contains((J, s)) && cards.contains((v10, s))
  }

  case class HandInfo(hand: Hand, players: Int)
  case class DealInfo(cut: Int, seconds: List[Int])
}