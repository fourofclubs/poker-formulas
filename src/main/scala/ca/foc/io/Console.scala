package ca.foc.io

import scala.io.StdIn._

import ca.foc.play.par.Par
import ca.foc.play.monads.Monad

sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
}

case object ReadLine extends Console[Option[String]] {
  def toPar = Par.lazyUnit(run)
  def toThunk = () => run

  def run: Option[String] = try { Some(readLine()) } catch { case e: Exception => None }
}

case class PrintLine(line: String) extends Console[Unit] {
  def toPar = Par.lazyUnit(println(line))
  def toThunk = () => println(line)
}

object Console {
  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
  def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

  val consoleToFunction0 = new (Console ~> Function0) { def apply[A](a: Console[A]) = a.toThunk }
  val consoleToPar = new (Console ~> Par) { def apply[A](a: Console[A]) = a.toPar }

  def runConsolePar[A](a: ConsoleIO[A]): Par[A] = runFree[Console, Par, A](a)(consoleToPar)
  def runConsole[A](a: ConsoleIO[A]): A = runTrampoline(translate(a)(consoleToFunction0))
}
