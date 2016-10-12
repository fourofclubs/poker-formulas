package ca.foc.mem

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
        if (index > 0) find(d.drop(players), ps2, players).map(index + 1 :: _)
        else List()
      }
  }

  val any = (c: Card) ⇒ true
  def isValue(v: CardVal) = (c: Card) ⇒ c.value == v
  def FH(v1: CardVal, v2: CardVal) = List(v1, v1, v1, v2, v2)
  val fullHouse = for (
    v1 ← VALUES;
    v2 ← VALUES.filterNot(_ == v1);
    p ← FH(v1, v2).permutations.toSet[List[CardVal]]
  ) yield v1 + "/" + v2 -> p.map(isValue(_))
  def Quad(v: CardVal) = List(Some(v), Some(v), Some(v), Some(v), None)
  val quads = for (
    v ← VALUES;
    p ← Quad(v).permutations.toSet[List[Option[CardVal]]]
  ) yield v + "s" -> p.map(_.map(isValue(_)).getOrElse(any))
  def SF(s: Suit, highVal: CardVal) =
    CARDS.filter(c ⇒ c.suit == s && ((highVal.intVal - 4) to highVal.intVal).contains(c.value.intVal))
  val straightFlush = for (
    s ← SUITS;
    v ← VALUES;
    sf ← SF(s, v).toList.permutations.toSet;
    if (v.intVal >= 5)
  ) yield "SF(" + s + ")" -> sf.map(c ⇒ (c2: Card) ⇒ c == c2)

  val seconds = {
    for (
      cut ← 0 to 51;
      d ← List(MemDeck.cut(cut));
      s ← fullHouse;
      players ← 3 to 10;
      seconds ← find(d, s._2, players)
    ) yield (s._1, cut, players) -> seconds
  }.sortBy(_._1._3).sortBy(_._1._1)

  //  for (s ← seconds)
  //    println(s._1 + ": " + s._2)

  println(fullHouse)
}
