package ca.foc.mem

object memdeck {
  def acaan(c: Card, n: Int): Card = {
    val m = MemDeck.positionOf(c)
    val cut = ((m - n + 51) % 51) + 1
    MemDeck.cut(cut).bottom
  }                                               //> acaan: (c: ca.foc.mem.Card, n: Int)ca.foc.mem.Card
  acaan((K, S), 52)                               //> res0: ca.foc.mem.Card = KS
  acaan((2, D), 52)                               //> res1: ca.foc.mem.Card = KS
  acaan((4, H), 52)                               //> res2: ca.foc.mem.Card = 4H
  acaan((8, D), 52)                               //> res3: ca.foc.mem.Card = 8D
  acaan((Q, C), 52)                               //> res4: ca.foc.mem.Card = QC
  acaan((6, H), 52)                               //> res5: ca.foc.mem.Card = 6H

}