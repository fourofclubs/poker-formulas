package ca.fourofclubs.playground
import ca.fourofclubs.playground._

case class Gen[A](sample: State[RNG, A])

object Gens {

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(Rand.nonNegativeLessThan(stopExclusive - start).map { _ + start + 1 })
  def unit[A](a: => A): Gen[A] = Gen[A](State.unit(a))
  def boolean: Gen[Boolean] = Gen[Boolean](Rand.nonNegativeLessThan(2).map { _ == 1 })
  def listOf[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(g.sample.times(n).map { _.toIterable.toList })

  val rng = SimpleRNG(2)
  choose(-2, 10).sample.times(20).run(rng)
  unit("5").sample.times(20).run(rng)
  boolean.sample.times(20).run(rng)
  listOf[Int](10, choose(-1, 10)).sample.times(4).run(rng)
  choose(-2, 10).sample.times(2).map { l => (l(0), l(1)) }.run(rng)
  choose(-2, 10).sample.map { Some(_) }.run(rng)
  listOf(100, choose(96, 122)).sample.map { _.map { _.toChar }.mkString }.run(rng)

}
