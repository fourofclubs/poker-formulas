package ca.foc.mem

object memdeck {
  def acaan(c: Card, n: Int): Card = {
    val m = MemDeck.positionOf(c)
    val cut = ((m - n + 51) % 51) + 1
    MemDeck.cut(cut).bottom
  }                                               //> acaan: (c: ca.foc.mem.Card, n: Int)ca.foc.mem.Card
  acaan((2, D), 52)                               //> res0: ca.foc.mem.Card = KS
  acaan((4, H), 52)                               //> res1: ca.foc.mem.Card = 4H
  acaan((8, D), 52)                               //> res2: ca.foc.mem.Card = 8D
  acaan((Q, C), 52)                               //> res3: ca.foc.mem.Card = QC
  acaan((6, H), 52)                               //> res4: ca.foc.mem.Card = 6H

  NewDeck                                         //> res5: ca.foc.mem.Deck = Deck(List(AH, 2H, 3H, 4H, 5H, 6H, 7H, 8H, 9H, 10H, J
                                                  //| H, QH, KH, AC, 2C, 3C, 4C, 5C, 6C, 7C, 8C, 9C, 10C, JC, QC, KC, KD, QD, JD, 
                                                  //| 10D, 9D, 8D, 7D, 6D, 5D, 4D, 3D, 2D, AD, KS, QS, JS, 10S, 9S, 8S, 7S, 6S, 5S
                                                  //| , 4S, 3S, 2S, AS))
  MemDeck                                         //> res6: ca.foc.mem.Deck = Deck(List(KS, 4H, KH, 8C, 9D, 6S, 8D, 3S, 10H, 5C, K
                                                  //| C, QS, 5H, 9S, 7H, 2C, 10C, 5D, 2S, 4D, 2H, QH, 7C, JD, 8S, 10D, 5S, 9H, 4C,
                                                  //|  QC, AD, 3H, JS, 6H, AC, 9C, 7D, 4S, 6D, AS, JH, 6C, KD, 10S, QD, 7S, 8H, 3C
                                                  //| , JC, 3D, AH, 2D))

  NewDeck.dealSecond                              //> res7: (ca.foc.mem.Card, ca.foc.mem.Deck) = (2H,Deck(List(AH, 3H, 4H, 5H, 6H,
                                                  //|  7H, 8H, 9H, 10H, JH, QH, KH, AC, 2C, 3C, 4C, 5C, 6C, 7C, 8C, 9C, 10C, JC, Q
                                                  //| C, KC, KD, QD, JD, 10D, 9D, 8D, 7D, 6D, 5D, 4D, 3D, 2D, AD, KS, QS, JS, 10S,
                                                  //|  9S, 8S, 7S, 6S, 5S, 4S, 3S, 2S, AS)))

}