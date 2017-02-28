package ca.foc.play.streaming

import scala.Stream
import scala.language.implicitConversions

import Process._
import scalaz.effect.IO
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

  import ca.foc.play.streaming.GeneralStreams._
  //  val file = new java.io.File("W:/2016-11-09-scala/git/scala-playground/temperature.txt")
  //  val fileP = filter((s: String) => !s.isEmpty() && !s.startsWith("#")) |>
  //    lift((s: String) => s.toDouble) |> lift(toCelcius)
  //  println(fileP)
  //  println(file)
  //  val pf = processFile(file, fileP, List[Double]())(_ :+ _)
  //  println(pf.unsafePerformIO())
  //
  //  import java.io.{ BufferedReader, FileReader }
  //  val ioP: GeneralStreams.Process[IO, String] =
  //    await(IO(new BufferedReader(new FileReader("lines.txt")))) {
  //      case Right(b) =>
  //        lazy val next: GeneralStreams.Process[IO, String] = await(IO(b.readLine)) {
  //          case Left(e) => await(IO(b.close))(_ => Halt(e))
  //          case Right(line) =>
  //            if (line eq null) Halt(End)
  //            else Emit(line, next)
  //        }
  //        next
  //      case Left(e) => Halt(e)
  //    }
  //  val log = runLog(ioP)
  //  println(log.unsafePerformIO())
  //
  //  println(ioP.runLog.unsafePerformIO())
  //
  //  println((lines("lines.txt") ++ lines("lines.txt")).runLog.unsafePerformIO())
  //
  //  val ioP2 = lines("lines.txt")
  //  println(ioP2)
  //
  //  println(ioP2.runLog.unsafePerformIO())

  //val converter = lines("temperature.txt").filter(_.startsWith("#"))

  val f = GeneralStreams.filter[String] { x => true }
  val r = resource(IO { io.Source.fromFile("temperature.txt") }) {
    src =>
      lazy val iter = src.getLines
      def step = if (iter.hasNext) Some(iter.next) else None
      lazy val lines: Process[IO, String] = eval(IO(step)).flatMap {
        case None       => Halt(End)
        case Some(line) => Emit(line, lines)
      }
      lines
  } { src => eval_ { IO(src.close) } }
  println(r.runLog.unsafePerformIO())

  val t = liftOne[String, String] { _.toUpperCase() }

  val s = r |> t
  println(s.runLog.unsafePerformIO())

  val u = lines("temperature.txt").
    filter(!_.startsWith("#")).
    map(line => toCelcius(line.toDouble).toString).
    pipe(intersperse("\n")).
    to(fileW("celsius.txt")).
    drain
  try { println(u.runLog.unsafePerformIO()) } catch { case e: Any => println(e.getClass) }

  //  println(converter.runLog.unsafePerformIO())
}