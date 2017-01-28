package ca.foc.mem

import ca.foc.play.random.RNG
import ca.foc.play.random.Rand._
import ca.foc.play.State

case class Mat(val deck: Deck, val selections: Map[String, Card]) {
  def selection(name: String) = selections.get(name)
  def select(name: String) = Mat.select(this, name)
  def replace(name: String, location: Int) = Mat.replace(this, name, location)
  def apply(f: Deck => Deck): Mat = Mat(this, f)
}

object Mat {
  def apply(d: Deck): Mat = Mat(d, Map[String, Card]())
  def select(m: Mat, name: String): State[RNG, Mat] = State((rng: RNG) => {
    val (n, rng2) = nonNegativeLessThan(m.deck.size).map(_ + 1)(rng)
    val (d, c) = m.deck.remove(n)
    (Mat(d, m.selections + (name -> c)), rng2)
  })
  def replace(m: Mat, name: String, location: Int): Mat =
    m.selection(name).map(s => Mat(m.deck.insert(s, location), m.selections - name))
      .getOrElse(throw new IllegalArgumentException(s"No selection named $name"))
  def apply(m: Mat, f: Deck => Deck): Mat = Mat(f(m.deck), m.selections)
}