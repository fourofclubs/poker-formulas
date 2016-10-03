package ca.foc.io

import ca.foc.play.monads.MonadOps
import ca.foc.play.monads.Monad
import Console._
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import com.google.common.util.concurrent.Futures
import com.google.common.util.concurrent.MoreExecutors._
import java.util.concurrent.TimeUnit._

object Test extends App {
  val es = Executors.newCachedThreadPool
  val f = Test2.run(es)
  Test1.run

  f.get
  shutdownAndAwaitTermination(es, 15, SECONDS)
}

object Test1 {
  case class Player(name: String, score: Int)
  def winner(p1: Player, p2: Player) =
    if (p1.score > p2.score) Some(p1)
    else if (p2.score > p1.score) Some(p2)
    else None
  def winnerMsg(p: Option[Player]) = p.map {
    case Player(name, _) => s"$name is the winner!"
  }.getOrElse("It's a draw.")
  def contest(p1: Player, p2: Player) = printLn(winnerMsg(winner(p1, p2)))

  def run = runConsole {
    contest(Player("Brad", 10), Player("Danyelle", 3)) ++
      contest(Player("Danyelle", 5), Player("Brad", 4))
  }
}

object Test2 {
  def fahrenheitToCelcius(f: Double) = (f - 32) * 5.0 / 9.0
  def converter = for {
    _ <- printLn("Enter a temperature in Fahrenheit:")
    d <- readLn.map(_.getOrElse("").toDouble)
    - <- printLn(fahrenheitToCelcius(d).toString)
  } yield ()

  def run(es: ExecutorService) = runConsolePar(converter)(es)
}

object Test4 {
  def run = {
    import Console.ConsoleIO
    try {
      val p = printLn("Still going...").forever
      runConsole(p)
    } catch {
      case e: Error => println("Error")
    }
  }
}