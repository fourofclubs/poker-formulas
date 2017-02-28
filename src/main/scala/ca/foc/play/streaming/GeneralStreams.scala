package ca.foc.play.streaming

import scala.language.higherKinds

import ca.foc.play.io.toMonadOps
import ca.foc.play.monads.Monad
import ca.foc.play.monads.MonadOps
import scalaz.effect.IO
import scalaz.effect.IOMonad
import java.io.FileWriter

object GeneralStreams {
  trait Process[F[_], O] {
    def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match {
      case Halt(e)          => Try(f(e))
      case Emit(h, t)       => Emit(h, t.onHalt(f))
      case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
    }
    def ++(p: => Process[F, O]): Process[F, O] = this.onHalt {
      case End => Try(p)
      case err => Halt(err)
    }
    def onComplete(p: => Process[F, O]): Process[F, O] = this.onHalt {
      case End => p.asFinalizer
      case err => p.asFinalizer ++ Halt(err)
    }
    def asFinalizer: Process[F, O] = this match {
      case Emit(h, t) => Emit(h, t.asFinalizer)
      case Halt(e)    => Halt(e)
      case Await(req, recv) => await(req) {
        case Left(Kill) => this.asFinalizer
        case x          => recv(x)
      }
    }
    def drain[O2]: Process[F, O2] = this match {
      case Halt(err)        => Halt(err)
      case Emit(h, t)       => t.drain
      case Await(req, recv) => Await(req, recv andThen (_.drain))
    }
    def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] = this match {
      case Halt(err)        => Halt(err)
      case Emit(o, t)       => Try(f(o)) ++ t.flatMap(f)
      case Await(req, recv) => Await(req, recv andThen (_ flatMap f))
    }
    def map[O2](f: O => O2): Process[F, O2] = flatMap { o => emit(f(o)) }
    def pipe[O2](p2: Process1[O, O2]): Process[F, O2] = this |> p2
    def runLog(implicit F: MonadCatch[F]): F[IndexedSeq[O]] = {
      def go(cur: Process[F, O], acc: IndexedSeq[O]): F[IndexedSeq[O]] = cur match {
        case Emit(h, t)       => go(t, acc :+ h)
        case Halt(End)        => F.unit(acc)
        case Halt(err)        => F.fail(err)
        case Await(req, recv) => F.flatMap(F.attempt(req)) { e => go(Try(recv(e)), acc) }
      }
      go(this, IndexedSeq())
    }
    def repeat: Process[F, O] = this ++ this.repeat
    def |>[O2](p2: => Process1[O, O2]): Process[F, O2] = {
      p2 match {
        case Halt(e)    => { println("? |> HALT"); this.kill onHalt { e2 => Halt(e) ++ Halt(e2) } }
        case Emit(h, t) => Emit(h, this |> t)
        case Await(req, recv) => this match {
          case Halt(err)          => { println("HALT |> AWAIT"); Halt(err) |> recv(Left(err)) }
          case Emit(h, t)         => t |> Try(recv(Right(h)))
          case Await(req0, recv0) => await(req0)(recv0 andThen (_ |> p2))
        }
      }
    }
    @annotation.tailrec
    final def kill[O2]: Process[F, O2] = this match {
      case Await(req, recv) => recv(Left(Kill)).drain.onHalt {
        case Kill => Halt(End)
        case e    => Halt(e)
      }
      case Halt(e)    => Halt(e)
      case Emit(h, t) => t.kill
    }
    def filter(f: O => Boolean) = this |> GeneralStreams.filter(f)
    def tee[O2, O3](p2: Process[F, O2])(t: Tee[O, O2, O3]): Process[F, O3] =
      t match {
        case Halt(e)    => this.kill onComplete p2.kill onComplete Halt(e)
        case Emit(h, t) => Emit(h, (this tee p2)(t))
        case Await(side, recv) => side.get match {
          case Left(isO) => this match {
            case Halt(e)     => p2.kill onComplete Halt(e)
            case Emit(o, ot) => (ot tee p2)(Try(recv(Right(o))))
            case Await(reqL, recvL) =>
              await(reqL)(recvL andThen (this2 => this2.tee(p2)(t)))
          }
          case Right(isO2) => p2 match {
            case Halt(e)      => this.kill onComplete Halt(e)
            case Emit(o2, ot) => (this tee ot)(Try(recv(Right(o2))))
            case Await(reqR, recvR) =>
              await(reqR)(recvR andThen (p3 => this.tee(p3)(t)))
          }
        }
      }
    def zipWith[O2, O3](p2: Process[F, O2])(f: (O, O2) => O3): Process[F, O3] =
      (this tee p2)(GeneralStreams.zipWith(f))
    def to[O2](sink: Sink[F, O]): Process[F, Unit] = join { (this zipWith sink)((o, f) => f(o)) }
  }

  trait MonadCatch[F[_]] extends Monad[F] {
    def attempt[A](a: F[A]): F[Either[Throwable, A]]
    def fail[A](t: Throwable): F[A]
  }

  implicit object IOMonad extends Monad[IO] {
    def unit[A](a: => A): IO[A] = IO(a)
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
  }

  implicit object IOMonadCatch extends MonadCatch[IO] {
    def attempt[A](a: IO[A]): IO[Either[Throwable, A]] = IO {
      try Right(a.unsafePerformIO()) catch { case err: Throwable => Left(err) }
    }
    def fail[A](t: Throwable): IO[A] = IO { throw t }
    def unit[A](a: => A) = IOMonad.unit(a)
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = IOMonad.flatMap(fa)(f)
  }

  case class Await[F[_], A, O](req: F[A], recv: Either[Throwable, A] => Process[F, O]) extends Process[F, O]
  case class Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]
  case class Halt[F[_], O](err: Throwable) extends Process[F, O]
  case object End extends Exception
  case object Kill extends Exception
  def Try[F[_], O](p: => Process[F, O]): Process[F, O] =
    try p
    catch { case e: Throwable => Halt(e) }
  def await[F[_], A, O](req: F[A])(recv: Either[Throwable, A] => Process[F, O]): Process[F, O] = Await(req, recv)

  def runLog[O](src: Process[IO, O]): IO[IndexedSeq[O]] = IO {
    @annotation.tailrec
    def go(cur: Process[IO, O], acc: IndexedSeq[O]): IndexedSeq[O] = cur match {
      case Emit(h, t) => go(t, acc :+ h)
      case Halt(End)  => acc
      case Halt(err)  => throw err
      case Await(req, recv) =>
        val next = recv { try Right(req.unsafePerformIO()) catch { case err: Throwable => Left(err) } }
        go(next, acc)
    }
    go(src, IndexedSeq())
  }
  def resource[R, O](acquire: IO[R])(use: R => Process[IO, O])(release: R => Process[IO, O]): Process[IO, O] =
    await[IO, R, O](acquire) {
      case Right(r) => use(r).onComplete(release(r))
      case Left(e)  => Halt(e)
    }
  def eval[F[_], A](a: F[A]): Process[F, A] = await[F, A, A](a) {
    case Right(a)  => emit(a)
    case Left(err) => Halt(err)
  }
  def eval_[F[_], A, B](a: F[A]): Process[F, B] = eval(a).drain[B]

  def lines(filename: String): Process[IO, String] = resource(IO { println("Loading file"); io.Source.fromFile(filename) }) {
    src =>
      lazy val iter = src.getLines
      def step = if (iter.hasNext) Some(iter.next) else None
      lazy val lines: Process[IO, String] = eval(IO(step)).flatMap {
        case None       => { println("EOF"); Halt(End) }
        case Some(line) => { println(line); Emit(line, lines) }
      }
      lines
  } { src => eval_ { println("closing file"); IO(src.close) } }

  def await1[I, O](recv: I => Process1[I, O], fallback: => Process1[I, O] = halt1[I, O]): Process1[I, O] =
    Await(Is[I].Get, (e: Either[Throwable, I]) => e match {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(i)  => Try(recv(i))
    })
  def emit[F[_], O](h: O, t1: Process[F, O] = Halt[F, O](End)): Process[F, O] = Emit(h, t1)
  def emit1[I, O](h: O, t1: Process1[I, O] = halt1[I, O]): Process1[I, O] = Emit(h, t1)
  def halt1[I, O]: Process1[I, O] = Halt[Is[I]#f, O](End)
  def liftOne[I, O](f: I => O): Process1[I, O] = await1[I, O](i => emit1(f(i)))
  def lift[I, O](f: I => O): Process1[I, O] = liftOne(f).repeat
  def filter[I](f: I => Boolean): Process1[I, I] = await1[I, I](i => if (f(i)) { println("Not filtered"); emit1(i) } else { println("filtered"); halt1 }).repeat
  def haltT[I, I2, O]: Tee[I, I2, O] = Halt[T[I, I2]#f, O](End)
  def awaitL[I, I2, O](recv: I => Tee[I, I2, O], fallback: => Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] =
    await[T[I, I2]#f, I, O](L) {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(a)  => Try(recv(a))
    }
  def awaitR[I, I2, O](recv: I2 => Tee[I, I2, O], fallback: => Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] =
    await[T[I, I2]#f, I2, O](R) {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(a)  => Try(recv(a))
    }
  def emitT[I, I2, O](h: O, t1: Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] = Emit(h, t1)
  def zipWith[I, I2, O](f: (I, I2) => O): Tee[I, I2, O] =
    awaitL[I, I2, O](i => awaitR(i2 => emitT(f(i, i2)))).repeat
  def zip[I, I2]: Tee[I, I2, (I, I2)] = zipWith((_, _))
  def join[F[_], O](p: Process[F, Process[F, O]]): Process[F, O] = p.flatMap { pa => pa }
  def intersperse[I](sep: I): Process1[I, I] =
    await1[I, I](i => emit1(i) ++ id.flatMap(i => emit1(sep) ++ emit1(i)))
  def id[I]: Process1[I, I] =
    await1((i: I) => emit(i, id))

  case class Is[I]() {
    sealed trait f[X]
    val Get = new f[I] {}
  }
  case class T[I, I2]() {
    sealed trait f[X] { def get: Either[I => X, I2 => X] }
    val L = new f[I] { def get = Left(identity) }
    val R = new f[I2] { def get = Right(identity) }
  }
  def L[I, I2] = T[I, I2]().L
  def R[I, I2] = T[I, I2]().R
  type Process1[I, O] = Process[Is[I]#f, O]
  type Sink[F[_], O] = Process[F, O => Process[F, Unit]]
  type Tee[I, I2, O] = Process[T[I, I2]#f, O]

  def fileW(file: String, append: Boolean = false): Sink[IO, String] =
    resource[FileWriter, String => Process[IO, Unit]] { IO { new FileWriter(file, append) } } {
      w => constant { (s: String) => eval[IO, Unit](IO(w.write(s))) }
    } { w => eval_(IO(w.close)) }
  def constant[A](a: A): Process[IO, A] = eval[IO, A](IO(a)).repeat
}
