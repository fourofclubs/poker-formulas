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
  }
  def int: Rand[Int] = State(_.nextInt)           //> int: => ca.fourofclubs.playground.States.Rand[Int]
  def nonNegativeInt: Rand[Int] = int.flatMap(n => {
    if (n != Int.MinValue) State.unit(Math.abs(n))
    else nonNegativeInt
  })                                              //> nonNegativeInt: => ca.fourofclubs.playground.States.Rand[Int]
  def nonNegativeLessThan(n: Int): Rand[Int] = nonNegativeInt.flatMap(i => {
    val mod = i % n
    if (i + (n - 1) - mod >= 0) State.unit(mod)
    else nonNegativeLessThan(n)
  })                                              //> nonNegativeLessThan: (n: Int)ca.fourofclubs.playground.States.Rand[Int]
  def double: Rand[Double] = nonNegativeInt.map(_.toDouble / Int.MaxValue)
                                                  //> double: => ca.fourofclubs.playground.States.Rand[Double]
  def intDouble: Rand[(Int, Double)] = State.both(int, double)
                                                  //> intDouble: => ca.fourofclubs.playground.States.Rand[(Int, Double)]
  def doubleInt: Rand[(Double, Int)] = State.both(double, int)
                                                  //> doubleInt: => ca.fourofclubs.playground.States.Rand[(Double, Int)]
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double.run(rng)
    val (d2, rng3) = double.run(rng2)
    val (d3, rng4) = double.run(rng3)
    ((d1, d2, d3), rng4)
  }                                               //> double3: (rng: ca.fourofclubs.playground.States.RNG)((Double, Double, Doubl
                                                  //| e), ca.fourofclubs.playground.States.RNG)
  def ints(count: Int): Rand[List[Int]] = State.sequence(List.fill(count)(int))
                                                  //> ints: (count: Int)ca.fourofclubs.playground.States.Rand[List[Int]]

  val rng = SimpleRNG(3)                          //> rng  : ca.fourofclubs.playground.States.SimpleRNG = SimpleRNG(3)
  doubleInt.run(rng)._1                           //> res0: (Double, Int) = (5.37487678480096E-4,832745806)

  nonNegativeLessThan(6).run(rng)._1              //> res1: Int = 2
  int.times(10).run(rng)._1                       //> res2: List[Int] = List(1154246, 832745806, -2005759122, -973416119, -148730
                                                  //| 4294, -387508411, -1093251575, -594271733, 1787214961, -1431003963)

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

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
  })                                              //> simulateMachine: (inputs: List[ca.fourofclubs.playground.States.Input])ca.f
                                                  //| ourofclubs.playground.States.State[ca.fourofclubs.playground.States.Machine
                                                  //| ,(Int, Int)]

  simulateMachine(List(Coin, Turn)).run(Machine(true, 10, 2))
                                                  //> res3: ((Int, Int), ca.fourofclubs.playground.States.Machine) = ((3,9),Machi
                                                  //| ne(true,9,3))
}