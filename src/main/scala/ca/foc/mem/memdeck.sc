package ca.foc.mem
import PokerFormulas._

object memdeck {
  def acaan(c: Card, n: Int): Card = {
    val m = MemDeck.positionOf(c)
    val cut = ((m - n + 51) % 51) + 1
    MemDeck.cut(cut).bottom
  }                                               //> acaan: (c: ca.foc.mem.Card, n: Int)ca.foc.mem.Card
  PokerFormulas.findHand(MemDeck, PokerFormulas.FullHouse(v3, v8), 8)
                                                  //> res0: Option[ca.foc.mem.PokerFormulas.DealInfo] = Some(DealInfo(23,List(2, 1
                                                  //| , 0, 1, 5)))\

}