package ca.foc.play.io

import scala.io.StdIn.readLine
import ca.foc.play.monads.{ Monad, MonadOps }
import ca.foc.play.par.Par

object IO2 {
  sealed trait Async[A] {
    def run = IO2.run(this)
    def map[B](f: A => B): Async[B] = flatMap(f andThen (Return(_)))
    def flatMap[B](f: A => Async[B]) = FlatMap(this, f)
    def ++[B](io: Async[B]): Async[B] = flatMap(a => io)
  }
  case class Return[A](a: A) extends Async[A]
  case class Suspend[A](resume: Par[A]) extends Async[A]
  case class FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

  object Async extends Monad[Async] {
    implicit def toMonadOps[A](io: Async[A]) = new MonadOps[Async, A](io, Async)
    def unit[A](a: => A) = Return(a)
    def flatMap[A, B](io: Async[A])(f: A => Async[B]): Async[B] = FlatMap(io, f)
    def apply[A](a: => A) = unit(a)
  }

  @annotation.tailrec def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(Return(x), f)     => step(f(x))
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (y => f(y) flatMap g))
    case _                         => async
  }
  def run[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a)  => Par.unit(a)
    case Suspend(r) => r
    case FlatMap(x, f) => x match {
      case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
      case _          => sys.error("Impossible; `step` eliminates these cases.")
    }
  }

  def PrintLine(s: String): Async[Unit] = Suspend(Par.lazyUnit(println(s)))
  val ReadLine: Async[String] = Suspend(Par.lazyUnit(readLine))
  val echo = ReadLine.flatMap(PrintLine(_))
  val readInt = ReadLine.map(_.toInt)
}