package ca.foc.io
import IO._
import scala.collection.TraversableOnce.MonadOps

object Test {
  def main(args: Array[String]) = {
    println("Test complete")
  }
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
  def contest(p1: Player, p2: Player) = PrintLine(winnerMsg(winner(p1, p2)))

  def run = {
    contest(Player("Brad", 10), Player("Danyelle", 3)) ++
      contest(Player("Danyelle", 5), Player("Brad", 4))
  }.run
}

object Test2 {
  def fahrenheitToCelcius(f: Double) = (f - 32) * 5.0 / 9.0
  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in Fahrenheit:")
    d <- ReadLine.map(_.toDouble)
    - <- PrintLine(fahrenheitToCelcius(d).toString)
  } yield ()

  def run = converter.run
}

object Test3 {
  def run = {
    val lines = echo * 5
    lines.run
  }
}