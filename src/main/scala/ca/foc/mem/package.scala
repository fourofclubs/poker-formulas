package ca.foc
import scala.collection.immutable.SortedMap

package object mem {
  sealed trait Suit
  object Spades extends Suit { override def toString = "S" }
  object Hearts extends Suit { override def toString = "H" }
  object Clubs extends Suit { override def toString = "C" }
  object Diamonds extends Suit { override def toString = "D" }
  val S = Spades
  val H = Hearts
  val C = Clubs
  val D = Diamonds

  sealed trait CardVal
  object K extends CardVal { override def toString = "K" }
  object Q extends CardVal { override def toString = "Q" }
  object J extends CardVal { override def toString = "J" }
  object v10 extends CardVal { override def toString = "10" }
  object v9 extends CardVal { override def toString = "9" }
  object v8 extends CardVal { override def toString = "8" }
  object v7 extends CardVal { override def toString = "7" }
  object v6 extends CardVal { override def toString = "6" }
  object v5 extends CardVal { override def toString = "5" }
  object v4 extends CardVal { override def toString = "4" }
  object v3 extends CardVal { override def toString = "3" }
  object v2 extends CardVal { override def toString = "2" }
  object A extends CardVal { override def toString = "A" }

  case class Card(value: CardVal, suit: Suit) { override def toString = value.toString + suit.toString }

  case class Deck(val cards: List[Card]) {
    def cardAt(n: Int) = cards(n - 1)
    def top = cards.head
    def bottom = cards.last
    def positionOf(c: Card) = cards.indexOf(c) + 1
  }
  object MemDeck extends Deck(List((K, S), (v4, H), (K, H), (v8, C), (v9, D), (v6, S), (v8, D), (v3, S), (v10, H),
    (v5, C), (K, C), (Q, S), (v5, H), (v9, S), (v7, H), (v2, C), (v10, C), (v5, D), (v2, S), (v4, D), (v2, H), (Q, H),
    (v7, C), (J, D), (v8, S), (v10, D), (v5, S), (v9, H), (v4, C), (Q, C), (A, D), (v3, H), (J, S), (v6, H), (A, C),
    (v9, C), (v7, D), (v4, S), (v6, D), (A, S), (J, H), (v6, C), (K, D), (v10, S), (Q, D), (v7, S), (v8, H), (v3, C),
    (J, C), (v3, D), (A, H), (v2, D)).map(toCard))

  def cut(d: Deck, n: Int) = Deck(d.cards.drop(n) ++ d.cards.take(n))

  case class DeckOps(d: Deck) {
    def cut(n: Int) = mem.cut(d, n)
  }

  private val valueIndex = Map(1 -> A, 2 -> v2, 3 -> v3, 4 -> v4, 5 -> v5, 6 -> v6, 7 -> v7, 8 -> v8,
    9 -> v9, 10 -> v10, 11 -> J, 12 -> Q, 13 -> K)
  implicit def toCard(c: (CardVal, Suit)) = Card(c._1, c._2)
  implicit def intToCard(c: (Int, Suit)) = Card(valueIndex(c._1), c._2)
  implicit def deckToDeckOps(d: Deck) = DeckOps(d)
}