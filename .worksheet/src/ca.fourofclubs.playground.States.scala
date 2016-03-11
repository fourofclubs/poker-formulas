package ca.fourofclubs.playground

import scala.annotation.tailrec

object States {
  case class State[S, +A](run: S => (A, S)) {
    def flatMap[B](g: A => State[S, B]): State[S, B] = State(s => {
      val (a, s2) = run(s)
      g(a).run(s2)
    })
    def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))
    def times(n: Int): State[S, List[A]] = State.sequence(List.fill(n)(this))
  }
  object State {
    def unit[S, A](a: A): State[S, A] = State((a, _))
    def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] =
      ra.flatMap(a => rb.flatMap(b => State.unit(f(a, b))))
    def both[S, A, B](ra: State[S, A], rb: State[S, B]): State[S, (A, B)] = map2(ra, rb)((_, _))
    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs match {
      case List() => State(s => (List[A](), s))
      case head :: tail => State(s => {
        val (a, s2) = head.run(s)
        val (as, s3) = sequence(tail).run(s2)
        (a :: as, s3)
      })
    }
  }

  type Rand[+A] = State[RNG, A]
  trait RNG {
    def nextInt: (Int, RNG)
  }
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  };import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(1384); 
  def int: Rand[Int] = State(_.nextInt);System.out.println("""int: => ca.fourofclubs.playground.States.Rand[Int]""");$skip(133); 
  def nonNegativeInt: Rand[Int] = int.flatMap(n => {
    if (n != Int.MinValue) State.unit(Math.abs(n))
    else nonNegativeInt
  });System.out.println("""nonNegativeInt: => ca.fourofclubs.playground.States.Rand[Int]""");$skip(182); 
  def nonNegativeLessThan(n: Int): Rand[Int] = nonNegativeInt.flatMap(i => {
    val mod = i % n
    if (i + (n - 1) - mod >= 0) State.unit(mod)
    else nonNegativeLessThan(n)
  });System.out.println("""nonNegativeLessThan: (n: Int)ca.fourofclubs.playground.States.Rand[Int]""");$skip(75); 
  def double: Rand[Double] = nonNegativeInt.map(_.toDouble / Int.MaxValue);System.out.println("""double: => ca.fourofclubs.playground.States.Rand[Double]""");$skip(63); 
  def intDouble: Rand[(Int, Double)] = State.both(int, double);System.out.println("""intDouble: => ca.fourofclubs.playground.States.Rand[(Int, Double)]""");$skip(63); 
  def doubleInt: Rand[(Double, Int)] = State.both(double, int);System.out.println("""doubleInt: => ca.fourofclubs.playground.States.Rand[(Double, Int)]""");$skip(203); 
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double.run(rng)
    val (d2, rng3) = double.run(rng2)
    val (d3, rng4) = double.run(rng3)
    ((d1, d2, d3), rng4)
  };System.out.println("""double3: (rng: ca.fourofclubs.playground.States.RNG)((Double, Double, Double), ca.fourofclubs.playground.States.RNG)""");$skip(80); 
  def ints(count: Int): Rand[List[Int]] = State.sequence(List.fill(count)(int));System.out.println("""ints: (count: Int)ca.fourofclubs.playground.States.Rand[List[Int]]""");$skip(27); 

  val rng = SimpleRNG(3);System.out.println("""rng  : ca.fourofclubs.playground.States.SimpleRNG = """ + $show(rng ));$skip(24); val res$0 = 
  doubleInt.run(rng)._1;System.out.println("""res0: (Double, Int) = """ + $show(res$0));$skip(39); val res$1 = 

  nonNegativeLessThan(6).run(rng)._1;System.out.println("""res1: Int = """ + $show(res$1));$skip(28); val res$2 = 
  int.times(10).run(rng)._1

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int);System.out.println("""res2: List[Int] = """ + $show(res$2));$skip(804); 

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(m => {
  	val unchanged = ((m.coins, m.candies), m)
    def takeInput(input: Input): State[Machine, (Int, Int)] = State(m => {
      if (m.candies <= 0) unchanged
      else input match {
        case Coin => if (m.locked) ((m.coins + 1, m.candies), Machine(false, m.candies, m.coins + 1)) else unchanged
        case Turn => if (m.locked) unchanged else ((m.coins, m.candies - 1), Machine(true, m.candies - 1, m.coins))
      }
    })
    inputs match {
      case List()  => unchanged
      case i :: is => simulateMachine(is).run(takeInput(i).run(m)._2)
    }
  });System.out.println("""simulateMachine: (inputs: List[ca.fourofclubs.playground.States.Input])ca.fourofclubs.playground.States.State[ca.fourofclubs.playground.States.Machine,(Int, Int)]""");$skip(64); val res$3 = 

  simulateMachine(List(Coin, Turn)).run(Machine(true, 10, 2));System.out.println("""res3: ((Int, Int), ca.fourofclubs.playground.States.Machine) = """ + $show(res$3))}
}
