package ca.foc.io

import scala.io.StdIn.readLine

import ca.foc.play.monads.Monad
import ca.foc.play.monads.MonadOps

sealed trait IO[A] { self =>
  def run: A
  def map[B](f: A => B): IO[B] = new IO[B] { def run = f(self.run) }
  def flatMap[B](f: A => IO[B]) = new IO[B] { def run = f(self.run).run }
  def ++[B](io: IO[B]): IO[B] = flatMap(a => io)
}
object IO extends Monad[IO] {
  implicit def toMonadOps[A](io: IO[A]) = new MonadOps[IO, A](io, IO)
  def unit[A](a: => A) = new IO[A] { def run = a }
  def flatMap[A, B](io: IO[A])(f: A => IO[B]): IO[B] = io.flatMap(f)
  def apply[A](a: => A) = unit(a)

  def PrintLine(msg: String) = IO { println(msg) }
  val ReadLine = IO { readLine }
  val echo = ReadLine.flatMap(PrintLine(_))
  val readInt = ReadLine.map(_.toInt)
}
