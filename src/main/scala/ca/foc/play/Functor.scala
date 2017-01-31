package ca.foc.play

trait Functor[F[_]] {
  def unit[A](a: A): F[A]
  def lift[A, B](f: A => B): F[A] => F[B]
  def map[A, B](fa: F[A], f: A => B) = lift(f)(fa)
}

sealed trait Opt[+A] {
  def isPresent = this match {
    case SomeOpt(_) => true
    case _          => false
  }
  def get: A = this match {
    case SomeOpt(a) => a
    case _          => throw new NoSuchElementException
  }
  def map[B](f: A => B) = OptFunctor.map(this, f)
}
case class SomeOpt[A](a: A) extends Opt[A]
case class EmptyOpt[A]() extends Opt[A]

object OptFunctor extends Functor[Opt] {
  def unit[A](a: A) = SomeOpt(a)
  def lift[A, B](f: A => B) = (oa: Opt[A]) => oa match {
    case SomeOpt(a) => SomeOpt(f(a))
    case EmptyOpt() => EmptyOpt[B]()
  }
}