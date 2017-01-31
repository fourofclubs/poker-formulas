package ca.foc.play.streaming

import ca.foc.play.monads.Monad
import java.io.File
import ca.foc.play.io.IO
import ca.foc.play.io.IO.TailRec

object Process {
  def liftOne[I, O](f: I => O): Process[I, O] = Await {
    case Some(i) => Emit(f(i))
    case None    => Halt()
  }
  def lift[I, O](f: I => O) = liftOne(f).repeat
  def filter[I](p: I => Boolean): Process[I, I] = Await[I, I] {
    case Some(i) if p(i) => Emit(i)
    case _               => Halt()
  }.repeat
  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] = Await {
      case Some(d) => Emit(d + acc, go(d + acc))
      case _       => Halt()
    }
    go(0.0)
  }
  def await[I, O](f: I => Process[I, O], fallback: Process[I, O] = Halt[I, O]()): Await[I, O] = Await {
    case Some(i) => f(i)
    case None    => fallback
  }
  def id[I]: Process[I, I] = await(Emit(_, id))
  def take[I](n: Int): Process[I, I] =
    if (n <= 0) Halt[I, I]()
    else await(Emit[I, I](_, take(n - 1)))
  def drop[I](n: Int): Process[I, I] =
    if (n > 0) await(_ => drop(n - 1))
    else await(Emit(_, id))
  def takeWhile[I](p: I => Boolean): Process[I, I] = Await {
    case Some(i) if p(i) => Emit(i, takeWhile(p))
    case _               => Halt()
  }
  def takeThrough[I](p: I => Boolean): Process[I, I] = Await {
    case Some(i) if p(i) => Emit(i, takeThrough(p))
    case Some(i)         => Emit(i)
    case None            => Halt()
  }
  def dropWhile[I](p: I => Boolean): Process[I, I] = Await {
    case Some(i) if p(i) => dropWhile(p)
    case Some(i)         => Emit(i, id)
    case None            => Halt()
  }
  def count[I]: Process[I, Int] = loop(1)((i: I, acc: Int) => (acc, acc + 1))
  def mean: Process[Double, Double] = loop((0, 0.0)) {
    case (d, (n, sum)) => ((sum + d) / (n + 1), (n + 1, sum + d))
  }
  def zip[I, O, O2](p1: Process[I, O], p2: Process[I, O2]): Process[I, (O, O2)] = (p1, p2) match {
    case (Halt(), _)                => Halt()
    case (_, Halt())                => Halt()
    case (Emit(b, t1), Emit(c, t2)) => Emit((b, c), zip(t1, t2))
    case (Await(recv1), _)          => Await(i => zip(recv1(i), feed(i)(p2)))
    case (_, Await(recv2))          => Await(i => zip(feed(i)(p1), recv2(i)))
  }
  def feed[I, O](i: Option[I])(p: Process[I, O]): Process[I, O] = p match {
    case Halt()      => p
    case Emit(h, t)  => Emit(h, feed(i)(t))
    case Await(recv) => recv(i)
  }
  def zipWithIndex[I]: Process[I, (I, Int)] = loop(0)((i: I, acc: Int) => ((i, acc), acc + 1))
  def mean2: Process[Double, Double] = (sum zip count) |> lift { case (s, n) => s / n }
  private def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
    await(i => f(i, z) match { case (o, s2) => Emit(o, loop(s2)(f)) })
  def monad[I]: Monad[({ type f[x] = Process[I, x] })#f] =
    new Monad[({ type f[x] = Process[I, x] })#f] {
      def unit[O](o: => O): Process[I, O] = Emit(o)
      def flatMap[O, O2](p: Process[I, O])(f: O => Process[I, O2]): Process[I, O2] = p flatMap f
    }
  def exists[I](f: I => Boolean): Process[I, Boolean] = lift(f) |> takeThrough(!_)
  def processFile[A, B](f: java.io.File, p: Process[String, A], z: B)(g: (B, A) => B): TailRec[B] = IO {
    @annotation.tailrec
    def go(ss: Iterator[String], cur: Process[String, A], acc: B): B =
      cur match {
        case Halt() => acc
        case Await(recv) =>
          val next = if (ss.hasNext) recv(Some(ss.next)) else recv(None)
          go(ss, next, acc)
        case Emit(h, t) => go(ss, t, g(acc, h))
      }
    val s = io.Source.fromFile(f)
    try go(s.getLines, p, z)
    finally s.close
  }
}

sealed trait Process[I, O] {
  import Process._
  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs      => recv(None)(xs)
    }
    case Emit(h, t) => h #:: t(s)
  }
  def repeat: Process[I, O] = {
    def go(p: Process[I, O]): Process[I, O] = p match {
      case Halt() => go(this)
      case Await(recv) => Await {
        case None => recv(None)
        case i    => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }
    go(this)
  }
  def |>[O2](p2: Process[_ >: O, O2]): Process[I, O2] = p2 match {
    case Halt()       => Halt()
    case Emit(h2, t2) => Emit(h2, this |> t2)
    case Await(recv2) => this match {
      case Halt()      => Halt() |> recv2(None)
      case Emit(h, t)  => t |> recv2(Some(h))
      case Await(recv) => Await(i => recv(i) |> p2)
    }
  }
  def map[O2](f: O => O2) = this |> lift(f)
  def ++(p: Process[I, O]): Process[I, O] = this match {
    case Halt()      => p
    case Emit(h, t)  => Emit(h, t ++ p)
    case Await(recv) => Await(recv andThen (_ ++ p))
  }
  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
    case Halt()      => Halt()
    case Emit(h, t)  => f(h) ++ (t flatMap f)
    case Await(recv) => Await(recv andThen (_ flatMap f))
  }
  def zip[O2](p: Process[I, O2]) = Process.zip(this, p)
}
case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]
case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]
case class Halt[I, O]() extends Process[I, O]