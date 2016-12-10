package ca.foc.mem

import scala.util.Random
import ca.foc.play.random.RNG

object Actions {
  def select(name: String): (Mat, RNG) => Mat = (m: Mat, rng: RNG) => {
    val n = rng.nextInt._1
    m.deck.cardAt(n)
    ???
  }
}