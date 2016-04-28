package ca.fourofclubs.playground.testing
import ca.fourofclubs.playground.testing._
import Gen._
import Prop._
import ca.fourofclubs.playground.random.SimpleRNG
import ca.fourofclubs.playground.par.Par
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

object Gens {

  val rng = SimpleRNG(20)                         //> rng  : ca.fourofclubs.playground.random.SimpleRNG = SimpleRNG(20)
  choose(-2, 10).sample.times(20).run(rng)        //> res0: (Seq[Int], ca.fourofclubs.playground.random.RNG) = (List(1, 5, 2, 3, 8
                                                  //| , 10, 9, 8, -1, -1, 6, 0, 0, 10, 8, 10, 7, 2, 6, -1),SimpleRNG(2138807525732
                                                  //| 0))
  unit("5").sample.times(20).run(rng)             //> res1: (Seq[String], ca.fourofclubs.playground.random.RNG) = (List(5, 5, 5, 5
                                                  //| , 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5),SimpleRNG(20))
  boolean.sample.times(20).run(rng)               //> res2: (Seq[Boolean], ca.fourofclubs.playground.random.RNG) = (List(false, fa
                                                  //| lse, true, false, true, true, false, true, false, false, true, true, true, t
                                                  //| rue, true, true, false, true, true, false),SimpleRNG(21388075257320))
  listOf[Int](10, choose(-1, 10)).sample.times(4).run(rng)
                                                  //> res3: (Seq[List[Int]], ca.fourofclubs.playground.random.RNG) = (List(List(5,
                                                  //|  6, 2, 9, 6, 8, 0, 0, 2, 7), List(8, 7, 10, 4, 4, 5, 4, 4, 10, 10), List(8, 
                                                  //| 4, 7, 0, 9, 8, 8, 5, 5, 7), List(0, 8, 1, 2, 9, 7, 8, 6, 5, 8)),SimpleRNG(17
                                                  //| 7222425252348))
  choose(-2, 10).sample.times(2).map { l => (l(0), l(1)) }.run(rng)
                                                  //> res4: ((Int, Int), ca.fourofclubs.playground.random.RNG) = ((1,5),SimpleRNG(
                                                  //| 174610480805614))
  choose(-2, 10).sample.map { Some(_) }.run(rng)  //> res5: (Some[Int], ca.fourofclubs.playground.random.RNG) = (Some(1),SimpleRNG
                                                  //| (504298078351))
  listOf(100, choose(96, 122)).sample.map { _.map { _.toChar }.mkString }.run(rng)
                                                  //> res6: (String, ca.fourofclubs.playground.random.RNG) = (sqzydtcxeyrtvhzzerru
                                                  //| kzhhpaealgmuvxfralouczawqlrooepfsfslayaisvtukoukkrcxjfugyhdvzzjvxasloprptaqp
                                                  //| ycoj,SimpleRNG(98241492282296))
  listOf(100, choose(-2, 2).flatMap { n => if (n < 0) boolean.map { _.toString } else unit("+") }).sample.run(rng)
                                                  //> res7: (List[String], ca.fourofclubs.playground.random.RNG) = (List(+, +, +, 
                                                  //| true, +, +, +, false, +, +, +, +, +, +, true, +, false, +, +, +, +, +, +, tr
                                                  //| ue, +, false, +, +, +, +, +, +, +, false, +, false, +, +, +, false, true, +,
                                                  //|  true, true, +, false, +, +, +, +, false, +, +, +, +, +, +, +, +, +, +, fals
                                                  //| e, +, +, +, +, +, +, +, +, false, +, true, +, +, +, false, +, +, +, true, +,
                                                  //|  +, false, +, +, +, +, +, +, +, false, +, +, +, +, +, +, +, +),SimpleRNG(161
                                                  //| 560273481447))
  listOf(10, choose(-10, 10).listOfN(choose(-2, 1))).sample.run(rng)._1
                                                  //> res8: List[List[Int]] = List(List(-7), List(), List(), List(), List(9), List
                                                  //| (), List(), List(), List(), List())
  listOf(10, union(choose(-10, -5), choose(5, 10))).sample.run(rng)
                                                  //> res9: (List[Int], ca.fourofclubs.playground.random.RNG) = (List(8, -8, -8, 6
                                                  //| , 9, -5, -9, -9, 10, -6),SimpleRNG(21388075257320))
  listOf(100, weighted(choose(-10, -5) -> 0.9, choose(5, 10) -> 0.1)).sample.run(rng)._1.count { _ > 0 }
                                                  //> res10: Int = 9

  val smallInt = choose(-10, 10)                  //> smallInt  : ca.fourofclubs.playground.testing.Gen[Int] = Gen(State(<functio
                                                  //| n1>))
  val maxProp = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }                                               //> maxProp  : ca.fourofclubs.playground.testing.Prop = Prop(<function3>)
  run(maxProp)                                    //> + OK, passed 100 tests.

  run(forAll(listOf1(choose(-100, 100))) { ns =>
    val sorted = ns.sorted
    sorted.head == ns.min && sorted.last == ns.max
  }, 10, 100, rng)                                //> + OK, passed 100 tests.

  run(check(1 == 1))                              //>  + OK, proved property

  run(forAllBooleans(b => b || !b))               //>  + OK, proved property
  run(forAllBooleans(b => b && !b))               //> ! Falsified after 0 passed tests:
                                                  //|  true
  run(forAll(-1000 to 1000 toSet)(n => {
    def sumDigits(m: Int): Int = {
      val sum = m.abs.toString.toList.map(_.toString.toInt).fold(0)(_ + _)
      if (sum.toString.length > 1) sumDigits(sum) else sum
    }
    n == 0 || sumDigits(n * 9) == 9
  }))                                             //>  + OK, proved property

	val ES = Executors.newCachedThreadPool    //> ES  : java.util.concurrent.ExecutorService = java.util.concurrent.ThreadPoo
                                                  //| lExecutor@2ab12ab1

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p, p2)(_ == _)
                                                  //> equal: [A](p: ca.fourofclubs.playground.par.Par[A], p2: ca.fourofclubs.play
                                                  //| ground.par.Par[A])ca.fourofclubs.playground.par.Par[Boolean]
  val p3 = Prop.check {
    equal(
    	Par.map(Par.unit(1))(_ + 1),
    	Par.unit(2)
    )(ES).get
  }                                               //> p3  : ca.fourofclubs.playground.testing.Prop = Prop(<function3>)
  run(p3)                                         //>  + OK, proved property
}