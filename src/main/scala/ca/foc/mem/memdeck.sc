package ca.foc.mem
import PokerFormulas._

object memdeck {
  def acaan(c: Card, n: Int): Card = {
    val m = MemDeck.positionOf(c)
    val cut = ((m - n + 51) % 51) + 1
    MemDeck.cut(cut).bottom
  }                                               //> acaan: (c: ca.foc.mem.Card, n: Int)ca.foc.mem.Card
  PokerFormulas.find(MemDeck.cut(37), List(isValue(K), isValue(v3), isValue(v3)), 6)
                                                  //> res0: List[List[Int]] = List(List(0, 5, 1), List(6, 5, 1))

  def test(d: Deck, players: Int) = Deck(d.cardAt(players) :: d.drop(players + 1).cards)
                                                  //> test: (d: ca.foc.mem.Deck, players: Int)ca.foc.mem.Deck
  val d = MemDeck.cut(43)                         //> d  : ca.foc.mem.Deck = Deck(List(10S, QD, 7S, 8H, 3C, JC, 3D, AH, 2D, KS, 4H
                                                  //| , KH, 8C, 9D, 6S, 8D, 3S, 10H, 5C, KC, QS, 5H, 9S, 7H, 2C, 10C, 5D, 2S, 4D, 
                                                  //| 2H, QH, 7C, JD, 8S, 10D, 5S, 9H, 4C, QC, AD, 3H, JS, 6H, AC, 9C, 7D, 4S, 6D,
                                                  //|  AS, JH, 6C, KD))
  d.cardAt(5)                                     //> res1: ca.foc.mem.Card = 3C
  d.cardAt(6)                                     //> res2: ca.foc.mem.Card = JC
  test(d, 6)                                      //> res3: ca.foc.mem.Deck = Deck(List(JC, AH, 2D, KS, 4H, KH, 8C, 9D, 6S, 8D, 3S
                                                  //| , 10H, 5C, KC, QS, 5H, 9S, 7H, 2C, 10C, 5D, 2S, 4D, 2H, QH, 7C, JD, 8S, 10D,
                                                  //|  5S, 9H, 4C, QC, AD, 3H, JS, 6H, AC, 9C, 7D, 4S, 6D, AS, JH, 6C, KD))
  PokerFormulas.find(MemDeck.cut(41), List(isValue(v3), isValue(v8), isValue(v3), isValue(v8), isValue(v3)), 9)
                                                  //> res4: List[List[Int]] = List()
  PokerFormulas.find(MemDeck.cut(25), List(isValue(v3)), 9)
                                                  //> res5: List[List[Int]] = List(List(7))
  PokerFormulas.find(MemDeck.cut(16), List(isValue(v8), isValue(v3)), 9)
                                                  //> res6: List[List[Int]] = List(List(0, 7), List(9, 7))
  PokerFormulas.find(MemDeck.cut(50), List(isValue(v8), isValue(v3), isValue(v8), isValue(v3)), 9)
                                                  //> res7: List[List[Int]] = List(List(0, 1, 0, 7), List(0, 1, 9, 7), List(6, 1, 
                                                  //| 0, 7), List(6, 1, 9, 7))
  MemDeck.cardAt(32)                              //> res8: ca.foc.mem.Card = 3H
  PokerFormulas.find(MemDeck.cut(41), List(isValue(v3), isValue(v8), isValue(v3), isValue(v8)), 9)
                                                  //> res9: List[List[Int]] = List(List(0, 0, 1, 0), List(0, 0, 1, 9), List(0, 6, 
                                                  //| 1, 0), List(0, 6, 1, 9), List(7, 0, 1, 0), List(7, 0, 1, 9), List(7, 6, 1, 0
                                                  //| ), List(7, 6, 1, 9))
  NewDeck                                         //> res10: ca.foc.mem.Deck = Deck(List(AH, 2H, 3H, 4H, 5H, 6H, 7H, 8H, 9H, 10H, 
                                                  //| JH, QH, KH, AC, 2C, 3C, 4C, 5C, 6C, 7C, 8C, 9C, 10C, JC, QC, KC, KD, QD, JD,
                                                  //|  10D, 9D, 8D, 7D, 6D, 5D, 4D, 3D, 2D, AD, KS, QS, JS, 10S, 9S, 8S, 7S, 6S, 5
                                                  //| S, 4S, 3S, 2S, AS))|

}