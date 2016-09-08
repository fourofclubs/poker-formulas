package ca.foc.play.random

import ca.foc.play.State

trait RNG {
  def nextInt: (Int, RNG)
}
case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
object Rand {
  def int: Rand[Int] = State(_.nextInt)
  def nonNegativeInt: Rand[Int] = int.flatMap(n ⇒ {
    if (n != Int.MinValue) State.unit(Math.abs(n))
    else nonNegativeInt
  })
  def nonNegativeLessThan(n: Int): Rand[Int] = nonNegativeInt.flatMap(i ⇒ {
    val mod = i % n
    if (i + (n - 1) - mod >= 0) State.unit(mod)
    else nonNegativeLessThan(n)
  })
  def double: Rand[Double] = nonNegativeInt.map(_.toDouble / Int.MaxValue)
  def intDouble: Rand[(Int, Double)] = State.both(int, double)
  def doubleInt: Rand[(Double, Int)] = State.both(double, int)
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double.run(rng)
    val (d2, rng3) = double.run(rng2)
    val (d3, rng4) = double.run(rng3)
    ((d1, d2, d3), rng4)
  }
  def ints(count: Int): Rand[Seq[Int]] = State.sequence(Seq.fill(count)(int))
}