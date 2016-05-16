package ca.fourofclubs.playground.monoids

import ca.fourofclubs.playground.testing.Prop
import ca.fourofclubs.playground.testing.Prop._
import ca.fourofclubs.playground.testing.Gen
import ca.fourofclubs.playground.par.Par

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
  def concatenate(as: List[A]) = Monoids.concatenate(as, this)
  def foldMap[B](as: List[B])(f: B => A) = Monoids.foldMap(as, this)(f)
}

object Monoids {
  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = concatenate(as.map(f), m)
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, endoMonoid[B])(f.curried)(z)
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v match {
    case Seq()  => m.zero
    case Seq(a) => f(a)
    case _ => {
      val (first, second) = v.splitAt(v.size / 2)
      m.op(foldMapV(first, m)(f), foldMapV(second, m)(f))
    }
  }
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
    def zero: Par[A] = Par.unit(m.zero)
  }
  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = foldMapV(v, par(m))(Par.asyncF(f))

  val intAddition = new Monoid[Int] {
    def op(n1: Int, n2: Int) = n1 + n2
    def zero = 0
  }
  val intMultiplication = new Monoid[Int] {
    def op(n1: Int, n2: Int) = n1 * n2
    def zero = 1
  }
  val booleanOr = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean) = b1 || b2
    def zero = false
  }
  val booleanAnd = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean) = b1 && b2
    def zero = true
  }
  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(o1: Option[A], o2: Option[A]) = o1 orElse o2
    def zero = None
  }
  def endoMonoid[A] = new Monoid[A => A] {
    def op(f1: A => A, f2: A => A) = a => f1(f2(a))
    def zero = a => a
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(gen) { a => m.op(a, m.zero) == a && m.op(m.zero, a) == a } &&
      forAll(gen.listOf(3)) { case List(a, b, c) => m.op(m.op(a, b), c) == m.op(a, m.op(b, c)) }
}