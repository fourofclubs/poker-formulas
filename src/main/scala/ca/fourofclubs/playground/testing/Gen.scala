package ca.fourofclubs.playground.testing

import ca.fourofclubs.playground.random.RNG
import ca.fourofclubs.playground.random.Rand
import ca.fourofclubs.playground.random.SimpleRNG
import ca.fourofclubs.playground.State
import ca.fourofclubs.playground.testing._
import ca.fourofclubs.playground.random.Rand
import ca.fourofclubs.playground.par.Par
import java.util.concurrent.Executors

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map { f })
  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = Gen(State.map2(sample, g.sample)(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap { a => f(a).sample })
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen.listOf(n, this))
  def unsized = SGen(_ => this)
  def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g)((_, _))
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] = SGen(forSize(_).map(f))
  def flatMap[B](f: A => Gen[B]): SGen[B] = SGen(forSize(_).flatMap(f))
  def apply(size: Int) = forSize(size)
  def **[B](s2: SGen[B]): SGen[(A, B)] = SGen(n => apply(n) ** s2(n))
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(Rand.nonNegativeLessThan(stopExclusive - start).map { _ + start + 1 })
  def unit[A](a: => A): Gen[A] = Gen[A](State.unit(a))
  def boolean: Gen[Boolean] = Gen[Boolean](Rand.nonNegativeLessThan(2).map { _ == 1 })
  def listOf[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(g.sample.times(n).map { _.toIterable.toList })
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(listOf(_, g))
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOf(n max 1, g))
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap { if (_) g1 else g2 }
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    Gen(Rand.double).flatMap { x => if (x * (g1._2.abs + g2._2.abs) < g1._2.abs) g1._1 else g2._1 }
  def intFn[A](g: Gen[Int]): Gen[A => Int] = g map (i => (a => a.hashCode >> i))
  def stringN(n: Int): Gen[String] = listOf(n, choose(0, 127)).map(_.map(_.toChar).mkString)
  val string: SGen[String] = SGen(stringN)
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (maxSize, n, rng) =>
    run(maxSize, n, rng) match {
      case Passed | Proved => p.run(maxSize, n, rng)
      case r               => r
    }
  }
  def ||(p: Prop): Prop = Prop { (maxSize, n, rng) =>
    run(maxSize, n, rng) match {
      case r: Falsified => p.run(maxSize, n, rng)
      case r            => r
    }
  }
}

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result { def isFalsified = false }
case object Proved extends Result { def isFalsified = false }
case class Falsified(failure: Prop.FailedCase, successes: Prop.SuccessCount) extends Result { def isFalsified = true }

object Prop {
  type FailedCase = String
  type SuccessCount = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (maxSize, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }.find(_.isFalsified).getOrElse(Passed)
  }
  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.iterate(g.sample.run(rng)) { case (a, rng) => g.sample.run(rng) }.map { case (a, rng) => a }
  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)
  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (maxSize, n, rng) =>
      {
        val casesPerSize = (n + (maxSize - 1)) / maxSize
        val props: Stream[Prop] = Stream.from(0).take((n min maxSize) + 1).map(i => forAll(g(i))(f))
        val prop: Prop = props.map(p => Prop((maxSize, _, rng) =>
          p.run(maxSize, casesPerSize, rng))).toList.reduce(_ && _)
        prop.run(maxSize, n, rng)
      }
  }
  def forAllBooleans(f: Boolean => Boolean): Prop = Prop { (_, _, _) =>
    if (!f(true)) Falsified("true", 0)
    else if (!f(false)) Falsified("false", 1)
    else Proved
  }
  def forAll[A](domain: Set[A])(f: A => Boolean) = Prop { (_, _, _) =>
    def check(d: List[A], count: Int): Result = d match {
      case List()  => Proved
      case a :: as => if (!f(a)) Falsified(a.toString, count) else check(as, count + 1)
    }
    check(domain.toList, 0)
  }
  private val S = Gen.weighted(Gen.choose(1, 4).map(Executors.newFixedThreadPool(_)) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25)
  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = forAll(S ** g) { case s ** a => f(a)(s).get }
  def check(p: => Boolean) = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = new SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed            => println(s"+ OK, passed $testCases tests.")
      case Proved            => println(s" + OK, proved property")
    }
}

object ** {
  def unapply[A, B](p: (A, B)) = Some(p)
}