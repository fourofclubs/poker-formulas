package ca.fourofclubs.playground

import scala.annotation.tailrec

object State {
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
  }
  case class State[S, +A](run: S => (A, S))
  type Rand[+A] = State[RNG, A];import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(492); 
  def int: Rand[Int] = _.nextInt;System.out.println("""int: => ca.fourofclubs.playground.State.Rand[Int]""");$skip(38); 
  def unit[A](a: A): Rand[A] = (a, _);System.out.println("""unit: [A](a: A)ca.fourofclubs.playground.State.Rand[A]""");$skip(78); 
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)));System.out.println("""map: [A, B](s: ca.fourofclubs.playground.State.Rand[A])(f: A => B)ca.fourofclubs.playground.State.Rand[B]""");$skip(122); 
  def flatMap[S, A, B](f: State[S, A])(g: A => State[S, B]): State[S, B] = s => {
    val (a, s2) = f(s)
    g(a)(s2)
  };System.out.println("""flatMap: [S, A, B](f: ca.fourofclubs.playground.State.State[S,A])(g: A => ca.fourofclubs.playground.State.State[S,B])ca.fourofclubs.playground.State.State[S,B]""");$skip(250); 
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case List() => rng => (List[A](), rng)
    case head :: tail => rng => {
      val (a, rng2) = head(rng)
      val (as, rng3) = sequence(tail)(rng2)
      (a :: as, rng3)
    }
  };System.out.println("""sequence: [A](fs: List[ca.fourofclubs.playground.State.Rand[A]])ca.fourofclubs.playground.State.Rand[List[A]]""");$skip(123); 
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))));System.out.println("""map2: [A, B, C](ra: ca.fourofclubs.playground.State.Rand[A], rb: ca.fourofclubs.playground.State.Rand[B])(f: (A, B) => C)ca.fourofclubs.playground.State.Rand[C]""");$skip(82); 

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _));System.out.println("""both: [A, B](ra: ca.fourofclubs.playground.State.Rand[A], rb: ca.fourofclubs.playground.State.Rand[B])ca.fourofclubs.playground.State.Rand[(A, B)]""");$skip(128); 
  def nonNegativeInt: Rand[Int] = flatMap(int)(n => {
    if (n != Int.MinValue) unit(Math.abs(n))
    else nonNegativeInt
  });System.out.println("""nonNegativeInt: => ca.fourofclubs.playground.State.Rand[Int]""");$skip(177); 
  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(i => {
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  });System.out.println("""nonNegativeLessThan: (n: Int)ca.fourofclubs.playground.State.Rand[Int]""");$skip(76); 
  def double: Rand[Double] = map(nonNegativeInt)(_.toDouble / Int.MaxValue);System.out.println("""double: => ca.fourofclubs.playground.State.Rand[Double]""");$skip(57); 
  def intDouble: Rand[(Int, Double)] = both(int, double);System.out.println("""intDouble: => ca.fourofclubs.playground.State.Rand[(Int, Double)]""");$skip(57); 
  def doubleInt: Rand[(Double, Int)] = both(double, int);System.out.println("""doubleInt: => ca.fourofclubs.playground.State.Rand[(Double, Int)]""");$skip(191); 
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  };System.out.println("""double3: (rng: ca.fourofclubs.playground.State.RNG)((Double, Double, Double), ca.fourofclubs.playground.State.RNG)""");$skip(74); 
  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int));System.out.println("""ints: (count: Int)ca.fourofclubs.playground.State.Rand[List[Int]]""");$skip(27); 

  val rng = SimpleRNG(5);System.out.println("""rng  : ca.fourofclubs.playground.State.SimpleRNG = """ + $show(rng ));$skip(17); val res$0 = 
  doubleInt(rng);System.out.println("""res0: <error> = """ + $show(res$0));$skip(32); val res$1 = 

  nonNegativeLessThan(6)(rng);System.out.println("""res1: <error> = """ + $show(res$1))}
}
