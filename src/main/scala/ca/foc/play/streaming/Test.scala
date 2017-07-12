package ca.foc.play.streaming

import scala.Stream
import scala.language.implicitConversions

import Process._
import scalaz.effect.IO
import scalaz.stream._
import ca.foc.play.streaming.GeneralStreams.IOMonadCatch

object Test extends App {
  val s1 = Stream.range(1, 10)
  val s2 = Stream(1.0, 5.5, 1.5, 6.0, 10.0, -11.0)

  val p = liftOne[Int, Int](_ + 7)
  val p2 = filter[Int](_ % 2 == 1)
  val p3 = sum
  val p4 = lift[Int, Int](_ * 4)

  //  println(count(s1).toList)
  //  println(mean(s2).toList)
  //  println(mean2(s2).toList)
  //  println((p3 |> lift(_ + 3))(s2).toList)
  //  println((lift[Int, Int](_ + 4) |> p2)(s1).toList)
  //
  //  println((lift((d: Double) => d * 3) |> mean |> zipWithIndex)(s2).toList)
  //  println(exists[Int](_ % 2 == 0)(s1).toList)
  //
  //  println(takeThrough[Int](_ < 2)(s1).toList)
  //
  def toCelcius(fahrenheit: Double): Double = (5.0 / 9.0) * (fahrenheit - 32)

  val task = io.linesR("temperature.txt").
    filter(!_.startsWith("#")).
    map(line => toCelcius(line.trim().toDouble).toString()).
    to(io.stdOutLines).
    drain.runLog
  println(task.run)

  import ca.foc.play.streaming.GeneralStreams._
  val converter = lines("temperature.txt").
    filter(!_.trim().startsWith("#")).
    flatMap(s => eval(IO.putStrLn(s).map(_ => s))).
    map(line => toCelcius(line.trim().toDouble).toString).
    pipe(intersperse("\n")).
    to(fileW("celsius.txt")).
    drain
  println(converter.runLog.unsafePerformIO())
}