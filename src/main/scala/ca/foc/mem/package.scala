package ca.foc

import scala.language.implicitConversions

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

  sealed trait CardVal extends Ordered[CardVal] {
    def intVal: Int
    override def compare(that: CardVal) = this.intVal.compare(that.intVal)
    def prev: CardVal
    def next: CardVal
    def -(n: Int): CardVal = if (n < 0) this + (-n) else if (n == 0) this else (prev - (n - 1))
    def +(n: Int): CardVal = if (n < 0) this - (-n) else if (n == 0) this else (next + (n - 1))
  }
  object K extends CardVal { override def toString = "K"; val intVal = 13; val prev = Q; val next = A }
  object Q extends CardVal { override def toString = "Q"; val intVal = 12; val prev = J; val next = K }
  object J extends CardVal { override def toString = "J"; val intVal = 11; val prev = v10; val next = Q }
  object v10 extends CardVal { override def toString = "10"; val intVal = 10; val prev = v9; val next = J }
  object v9 extends CardVal { override def toString = "9"; val intVal = 9; val prev = v8; val next = v10 }
  object v8 extends CardVal { override def toString = "8"; val intVal = 8; val prev = v7; val next = v9 }
  object v7 extends CardVal { override def toString = "7"; val intVal = 7; val prev = v6; val next = v8 }
  object v6 extends CardVal { override def toString = "6"; val intVal = 6; val prev = v5; val next = v7 }
  object v5 extends CardVal { override def toString = "5"; val intVal = 5; val prev = v4; val next = v6 }
  object v4 extends CardVal { override def toString = "4"; val intVal = 4; val prev = v3; val next = v5 }
  object v3 extends CardVal { override def toString = "3"; val intVal = 3; val prev = v2; val next = v4 }
  object v2 extends CardVal { override def toString = "2"; val intVal = 2; val prev = A; val next = v3 }
  object A extends CardVal { override def toString = "A"; val intVal = 1; val prev = K; val next = v2 }

  val SUITS = Set(S, H, C, D)
  val VALUES = Set(A, v2, v3, v4, v5, v6, v7, v8, v9, v10, J, Q, K)
  val CARDS = for (s ← SUITS; v ← VALUES) yield Card(v, s)

  case class Card(value: CardVal, suit: Suit) { override def toString = value.toString + suit.toString }

  case class Deck(val cards: List[Card]) {
    def cardAt(n: Int) = cards(n - 1)
    def top = cards.head
    def bottom = cards.last
    def positionOf(c: Card) = cards.indexOf(c) + 1
    def size = cards.size
    def drop(n: Int) = Deck(cards.drop(n))
    def cut(n: Int) = Deck(cards.drop(n) ++ cards.take(n))
    def deal: (Deck, Card) = (drop(1), top)
    def dealSecond: (Deck, Card) = (Deck(top :: drop(2).cards), cardAt(2))
    def dealBottom: (Deck, Card) = (Deck(cards.dropRight(1)), bottom)
    def remove(n: Int): (Deck, Card) = (Deck(cards.take(n - 1) ++ cards.drop(n + 1)), cardAt(n))
    def insert(card: Card, n: Int) = Deck(cards.take(n - 1) ++ (card :: cards.drop(n - 1)))
  }
  val NewDeck = Deck(((for (v <- VALUES.toList.sorted) yield (v, H)) ++
    (for (v <- VALUES.toList.sorted) yield (v, C)) ++
    (for (v <- VALUES.toList.sorted.reverse) yield (v, D)) ++
    (for (v <- VALUES.toList.sorted.reverse) yield (v, S))).map(toCard))
  val MemDeck = Deck(List((K, S), (v4, H), (K, H), (v8, C), (v9, D), (v6, S), (v8, D), (v3, S), (v10, H),
    (v5, C), (K, C), (Q, S), (v5, H), (v9, S), (v7, H), (v2, C), (v10, C), (v5, D), (v2, S), (v4, D), (v2, H), (Q, H),
    (v7, C), (J, D), (v8, S), (v10, D), (v5, S), (v9, H), (v4, C), (Q, C), (A, D), (v3, H), (J, S), (v6, H), (A, C),
    (v9, C), (v7, D), (v4, S), (v6, D), (A, S), (J, H), (v6, C), (K, D), (v10, S), (Q, D), (v7, S), (v8, H), (v3, C),
    (J, C), (v3, D), (A, H), (v2, D)))

  private val valueIndex = Map(1 -> A, 2 -> v2, 3 -> v3, 4 -> v4, 5 -> v5, 6 -> v6, 7 -> v7, 8 -> v8,
    9 -> v9, 10 -> v10, 11 -> J, 12 -> Q, 13 -> K)
  implicit def toCard(c: (CardVal, Suit)): Card = Card(c._1, c._2)
  implicit def intToCard(c: (Int, Suit)): Card = Card(valueIndex(c._1), c._2)
}