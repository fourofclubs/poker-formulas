package ca.foc.mem

import ca.foc.play.random.RNG
import ca.foc.play.random.SimpleRNG

object cardtracks {
  val rng = SimpleRNG(120)                        //> rng  : ca.foc.play.random.SimpleRNG = SimpleRNG(120)
  Mat(MemDeck).select("a").run(rng)._1            //> res0: ca.foc.mem.Mat = Mat(Deck(List(KS, 4H, KH, 8C, 8D, 3S, 10H, 5C, KC, QS
                                                  //| , 5H, 9S, 7H, 2C, 10C, 5D, 2S, 4D, 2H, QH, 7C, JD, 8S, 10D, 5S, 9H, 4C, QC, 
                                                  //| AD, 3H, JS, 6H, AC, 9C, 7D, 4S, 6D, AS, JH, 6C, KD, 10S, QD, 7S, 8H, 3C, JC,
                                                  //|  3D, AH, 2D)),Map(a -> 9D))
  Mat(MemDeck).select("a").map(_.replace("a", 3)).run(rng)._1
                                                  //> res1: ca.foc.mem.Mat = Mat(Deck(List(KS, 4H, 9D, KH, 8C, 8D, 3S, 10H, 5C, KC
                                                  //| , QS, 5H, 9S, 7H, 2C, 10C, 5D, 2S, 4D, 2H, QH, 7C, JD, 8S, 10D, 5S, 9H, 4C, 
                                                  //| QC, AD, 3H, JS, 6H, AC, 9C, 7D, 4S, 6D, AS, JH, 6C, KD, 10S, QD, 7S, 8H, 3C,
                                                  //|  JC, 3D, AH, 2D)),Map())
}