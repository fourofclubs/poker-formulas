package ca.fourofclubs.playground.testing
import ca.fourofclubs.playground.testing._
import Gen._
import Prop._
import ca.fourofclubs.playground.random.SimpleRNG
import ca.fourofclubs.playground.par.Par
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

object Gens {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(328); 

  val rng = SimpleRNG(20);System.out.println("""rng  : ca.fourofclubs.playground.random.SimpleRNG = """ + $show(rng ));$skip(43); val res$0 = 
  choose(-2, 10).sample.times(20).run(rng);System.out.println("""res0: (Seq[Int], ca.fourofclubs.playground.random.RNG) = """ + $show(res$0));$skip(38); val res$1 = 
  unit("5").sample.times(20).run(rng);System.out.println("""res1: (Seq[String], ca.fourofclubs.playground.random.RNG) = """ + $show(res$1));$skip(36); val res$2 = 
  boolean.sample.times(20).run(rng);System.out.println("""res2: (Seq[Boolean], ca.fourofclubs.playground.random.RNG) = """ + $show(res$2));$skip(59); val res$3 = 
  listOf[Int](10, choose(-1, 10)).sample.times(4).run(rng);System.out.println("""res3: (Seq[List[Int]], ca.fourofclubs.playground.random.RNG) = """ + $show(res$3));$skip(68); val res$4 = 
  choose(-2, 10).sample.times(2).map { l => (l(0), l(1)) }.run(rng);System.out.println("""res4: ((Int, Int), ca.fourofclubs.playground.random.RNG) = """ + $show(res$4));$skip(49); val res$5 = 
  choose(-2, 10).sample.map { Some(_) }.run(rng);System.out.println("""res5: (Some[Int], ca.fourofclubs.playground.random.RNG) = """ + $show(res$5));$skip(83); val res$6 = 
  listOf(100, choose(96, 122)).sample.map { _.map { _.toChar }.mkString }.run(rng);System.out.println("""res6: (String, ca.fourofclubs.playground.random.RNG) = """ + $show(res$6));$skip(115); val res$7 = 
  listOf(100, choose(-2, 2).flatMap { n => if (n < 0) boolean.map { _.toString } else unit("+") }).sample.run(rng);System.out.println("""res7: (List[String], ca.fourofclubs.playground.random.RNG) = """ + $show(res$7));$skip(72); val res$8 = 
  listOf(10, choose(-10, 10).listOfN(choose(-2, 1))).sample.run(rng)._1;System.out.println("""res8: List[List[Int]] = """ + $show(res$8));$skip(68); val res$9 = 
  listOf(10, union(choose(-10, -5), choose(5, 10))).sample.run(rng);System.out.println("""res9: (List[Int], ca.fourofclubs.playground.random.RNG) = """ + $show(res$9));$skip(105); val res$10 = 
  listOf(100, weighted(choose(-10, -5) -> 0.9, choose(5, 10) -> 0.1)).sample.run(rng)._1.count { _ > 0 };System.out.println("""res10: Int = """ + $show(res$10));$skip(35); 

  val smallInt = choose(-10, 10);System.out.println("""smallInt  : ca.fourofclubs.playground.testing.Gen[Int] = """ + $show(smallInt ));$skip(99); 
  val maxProp = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  };System.out.println("""maxProp  : ca.fourofclubs.playground.testing.Prop = """ + $show(maxProp ));$skip(15); 
  run(maxProp);$skip(148); 

  run(forAll(listOf1(choose(-100, 100))) { ns =>
    val sorted = ns.sorted
    sorted.head == ns.min && sorted.last == ns.max
  }, 10, 100, rng);$skip(23); 

  run(check(1 == 1));$skip(38); 

  run(forAllBooleans(b => b || !b));$skip(36); 
  run(forAllBooleans(b => b && !b));$skip(258); 
  run(forAll(-1000 to 1000 toSet)(n => {
    def sumDigits(m: Int): Int = {
      val sum = m.abs.toString.toList.map(_.toString.toInt).fold(0)(_ + _)
      if (sum.toString.length > 1) sumDigits(sum) else sum
    }
    n == 0 || sumDigits(n * 9) == 9
  }));$skip(41); 

	val ES = Executors.newCachedThreadPool;System.out.println("""ES  : java.util.concurrent.ExecutorService = """ + $show(ES ));$skip(80); 

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p, p2)(_ == _);System.out.println("""equal: [A](p: ca.fourofclubs.playground.par.Par[A], p2: ca.fourofclubs.playground.par.Par[A])ca.fourofclubs.playground.par.Par[Boolean]""");$skip(104); 
  val p3 = Prop.check {
    equal(
    	Par.map(Par.unit(1))(_ + 1),
    	Par.unit(2)
    )(ES).get
  };System.out.println("""p3  : ca.fourofclubs.playground.testing.Prop = """ + $show(p3 ));$skip(10); 
  run(p3)}
}
