package ca.fourofclubs.playground

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoids {
  val intAddition = new Monoid[Int] {
    def op(n: Int, m: Int) = n + m
    def zero = 0
  }
  val intMultiplication = new Monoid[Int] {
    def op(n: Int, m: Int) = n * m
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
    def op(o1: Option[A], o2: Option[A]) = o1.orElse(o2)
    def zero = None
  }
  def endoMonoid[A] = new Monoid[A ⇒ A] {
    def op(f: A ⇒ A, g: A ⇒ A) = a ⇒ f(g(a))
    def zero = a ⇒ a
  }
  def concatenate[A](as: List[A], m: Monoid[A]) = as.foldRight(m.zero)(m.op)
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A ⇒ B) = concatenate(as.map(f), m)
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A ⇒ B): B =
    if (as.size == 0) m.zero
    else if (as.size == 1) f(as.head)
    else {
      val (first, second) = as.splitAt(as.size / 2)
      m.op(foldMapV(first, m)(f), foldMapV(second, m)(f))
    }
  def ordered(as: IndexedSeq[Int]) = {
    val m = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(x1: Option[(Int, Int, Boolean)], x2: Option[(Int, Int, Boolean)]) = (x1, x2) match {
        case (Some((min1, max1, ordered1)), Some((min2, max2, ordered2))) ⇒
          Some(min1 min min2, max1 max max2, ordered1 && ordered2 && max1 <= min2)
        case (x, None) ⇒ x
        case (None, x) ⇒ x
      }
      val zero = None
    }
    foldMapV(as, m)(n ⇒ Some(n, n, true)).map(_._3).getOrElse(true)
  }
}