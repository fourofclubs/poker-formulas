package ca.foc.mem

object memdeck {
  def acaan(c: Card, n: Int): Card = {
    val m = MemDeck.positionOf(c)
    val cut = ((m - n + 51) % 51) + 1
    MemDeck.cut(cut).bottom
  }                                               //> acaan: (c: ca.foc.mem.Card, n: Int)ca.foc.mem.Card
  PokerFormulas.SF(S, v8)                         //> res0: scala.collection.immutable.Set[ca.foc.mem.Card] = Set(7S, 4S, 6S, 5S, 
                                                  //| 8S)
	MemDeck.cut(15)                           //> res1: ca.foc.mem.Deck = Deck(List(2C, 10C, 5D, 2S, 4D, 2H, QH, 7C, JD, 8S, 1
                                                  //| 0D, 5S, 9H, 4C, QC, AD, 3H, JS, 6H, AC, 9C, 7D, 4S, 6D, AS, JH, 6C, KD, 10S,
                                                  //|  QD, 7S, 8H, 3C, JC, 3D, AH, 2D, KS, 4H, KH, 8C, 9D, 6S, 8D, 3S, 10H, 5C, KC
                                                  //| , QS, 5H, 9S, 7H))

}