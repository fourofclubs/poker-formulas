package ca.foc.mem

import java.io.{ File, PrintStream }

object PokerFormulas extends App {
  def find[C >: Card](d: Deck, ps: List[C ⇒ Boolean], players: Int): List[List[Int]] = ps match {
    case List() ⇒ List(List())
    case p :: ps2 ⇒
      if (d.size < players) List()
      else if (p(d.cardAt(players))) find(d.drop(players), ps2, players).map(0 :: _)
      else if (d.size > players && p(d.cardAt(players + 1)))
        find(Deck(d.cardAt(players - 1) :: d.drop(players + 1).cards), ps2, players).map(players :: _)
      else {
        val index = d.cards.take(players).indexWhere(p)
        if (index >= 0) find(d.drop(players), ps2, players).map(index + 1 :: _)
        else List()
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

  val twoPair = for (
    v1 <- VALUES;
    v2 <- VALUES.filter(_.intVal < v1.intVal);
    p <- List(isValue(v1), isValue(v1), isValue(v2), isValue(v2), !isValue(v1) && !isValue(v2)).permutations.toSet[List[Card => Boolean]]
  ) yield v1 + "+" + v2 -> p
  val trips = for (
    v <- VALUES;
    p <- List(isValue(v), isValue(v), isValue(v), !isValue(v), !isValue(v)).permutations.toSet[List[Card => Boolean]]
  ) yield "T" + v -> p
  val straight = for (
    v <- VALUES.filter(v => !(2 to 4).contains(v.intVal));
    p <- List(isValue(v), isValue(v - 1), isValue(v - 2), isValue(v - 3), isValue(v - 4)).permutations.toSet[List[Card => Boolean]]
  ) yield "S(" + v + ")" -> p
  val flush = for (
    s <- SUITS;
    v <- VALUES.filter(_.intVal >= 6);
    p <- (isCard(v, s) :: List.fill(4)(isSuit(s) && isLower(v))).permutations.toSet[List[Card => Boolean]]
  ) yield "F(" + s + "," + v + ")" -> p
  val fullHouse = for (
    v1 ← VALUES;
    v2 ← VALUES.filterNot(_ == v1);
    p ← List(isValue(v1), isValue(v1), isValue(v1), isValue(v2), isValue(v2)).permutations.toSet[List[Card => Boolean]]
  ) yield v1 + "/" + v2 -> p
  val quads = for (
    v ← VALUES;
    p ← List(isValue(v), isValue(v), isValue(v), isValue(v), any).permutations.toSet[List[Card => Boolean]]
  ) yield v + "s" -> p
  val straightFlush = for (
    s ← SUITS;
    v ← VALUES.filter(_.intVal >= 5);
    p <- List.fill(5)(isSuit(s) && inRange(v.intVal - 4, v.intVal)).permutations.toSet[List[Card => Boolean]]
  ) yield "SF(" + s + "," + v + ")" -> p
  val royalFlush = for (
    s <- SUITS;
    p <- List(isCard(A, s), isCard(K, s), isCard(Q, s), isCard(J, s), isCard(v10, s)).permutations.toSet[List[Card => Boolean]]
  ) yield "RF(" + s + ")" -> p

  def evaluate(players: Int)(d: DealInfo) = (5.0 * players) - (d.seconds.sum + d.seconds.count(_ == 0) * players * 1.2)
  case class HandInfo(name: String, players: Int)
  case class DealInfo(cut: Int, seconds: List[Int])
  val seconds = {
    for (
      cut ← (0 to 51).par;
      d ← List(MemDeck.cut(cut));
      s ← (royalFlush ++ straightFlush ++ quads ++ fullHouse ++ flush ++ straight ++ trips ++ twoPair).par;
      players ← 3 to 10
    ) yield HandInfo(s._1, players) -> find(d, s._2, players).map(s => DealInfo(cut, s))
  }.toList.filter(!_._2.isEmpty).map(p => p._1 -> p._2.maxBy(evaluate(p._1.players)_)).toMap

  val file = new File("pokerFormulas.txt")
  file.delete()
  val ps = new PrintStream(file)

  for ((h, d) <- seconds.toList.sortBy(_._1.players).sortBy(_._1.name))
    ps.println(h.name + ", " + h.players + ": " + d.cut + "-" + d.seconds.mkString(","))
}
