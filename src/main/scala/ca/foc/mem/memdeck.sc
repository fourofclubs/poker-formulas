package ca.foc.mem

object memdeck {
  def acaan(c: Card, n: Int): Card = {
    val m = MemDeck.positionOf(c)
    val cut = ((m - n + 51) % 51) + 1
    MemDeck.cut(cut).bottom
  }                                               //> acaan: (c: ca.foc.mem.Card, n: Int)ca.foc.mem.Card
  PokerFormulas.SF(S, v8)                         //> res0: scala.collection.immutable.Set[ca.foc.mem.Card] = Set(4S, 8S, 6S, 5S, 
                                                  //| 7S)
}