package ca.foc.play.monoids

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

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]) = foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  def toList[A](fa: F[A]): List[A] = foldRight(fa)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
  override def toList[A](fa: List[A]) = fa
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
  def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]) = Monoids.foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) = as.foldRight(z)(f)
  def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) = as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(a)      => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
  def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a)      => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a)      => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }
}

object OptionFoldable extends Foldable[Option] {
  def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as.map(f(_, z)).getOrElse(z)
  def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as.map(f(z, _)).getOrElse(z)
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]) = as.map(f).getOrElse(mb.zero)
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
  def listConcatMonoid[A] = new Monoid[List[A]] {
    def op(l1: List[A], l2: List[A]) = l1 ++ l2
    def zero = List()
  }
  val wcMonoid = new Monoid[WC] {
    def op(wc1: WC, wc2: WC) = (wc1, wc2) match {
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if ((r1 + l2).isEmpty) 0 else 1), r2)
      case (Stub(s1), Part(l2, w2, r2))         => Part(s1 + l2, w2, r2)
      case (Part(l1, w1, r1), Stub(s2))         => Part(l1, w1, r1 + s2)
      case (Stub(s1), Stub(s2))                 => Stub(s1 + s2)
    }
    def zero = Stub("")
  }
  def wordCount(str: String) = foldMapV(str, wcMonoid)(c => if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)) match {
    case Stub(_)           => 1
    case Part(l, words, r) => (if (l.size > 0) 1 else 0) + words + (if (r.size > 0) 1 else 0)
  }
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]) = new Monoid[(A, B)] {
    def op(p1: (A, B), p2: (A, B)) = (A.op(p1._1, p2._1), B.op(p1._2, p2._2))
    def zero = (A.zero, B.zero)
  }
  def mapMergeMonoid[K, V](V: Monoid[V]) = new Monoid[Map[K, V]] {
    def op(m1: Map[K, V], m2: Map[K, V]) = (m1.keySet ++ m2.keySet).foldLeft(zero) { (acc, k) =>
      acc.updated(k, V.op(m1.getOrElse(k, V.zero), m2.getOrElse(k, V.zero)))
    }
    def zero = Map[K, V]()
  }
  def functionMonoid[A, B](B: Monoid[B]) = new Monoid[A => B] {
    def op(f: A => B, g: A => B) = a => B.op(f(a), g(a))
    def zero = a => B.zero
  }
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(gen) { a => m.op(a, m.zero) == a && m.op(m.zero, a) == a } &&
      forAll(gen.listOf(3)) { case List(a, b, c) => m.op(m.op(a, b), c) == m.op(a, m.op(b, c)) }
}