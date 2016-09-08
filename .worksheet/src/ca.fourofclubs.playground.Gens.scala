package ca.fourofclubs.playground
import ca.fourofclubs.playground._
import Gen._
import Prop._

object Gens {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(139); 

  val rng = SimpleRNG(20);System.out.println("""rng  : ca.fourofclubs.playground.SimpleRNG = """ + $show(rng ));$skip(43); val res$0 = 
  choose(-2, 10).sample.times(20).run(rng);System.out.println("""res0: <error> = """ + $show(res$0));$skip(38); val res$1 = 
  unit("5").sample.times(20).run(rng);System.out.println("""res1: <error> = """ + $show(res$1));$skip(36); val res$2 = 
  boolean.sample.times(20).run(rng);System.out.println("""res2: <error> = """ + $show(res$2));$skip(59); val res$3 = 
  listOf[Int](10, choose(-1, 10)).sample.times(4).run(rng);System.out.println("""res3: <error> = """ + $show(res$3));$skip(68); val res$4 = 
  choose(-2, 10).sample.times(2).map { l => (l(0), l(1)) }.run(rng);System.out.println("""res4: <error> = """ + $show(res$4));$skip(49); val res$5 = 
  choose(-2, 10).sample.map { Some(_) }.run(rng);System.out.println("""res5: <error> = """ + $show(res$5));$skip(83); val res$6 = 
  listOf(100, choose(96, 122)).sample.map { _.map { _.toChar }.mkString }.run(rng);System.out.println("""res6: <error> = """ + $show(res$6));$skip(115); val res$7 = 
  listOf(100, choose(-2, 2).flatMap { n => if (n < 0) boolean.map { _.toString } else unit("+") }).sample.run(rng);System.out.println("""res7: <error> = """ + $show(res$7));$skip(72); val res$8 = 
  listOf(10, choose(-10, 10).listOfN(choose(-2, 1))).sample.run(rng)._1;System.out.println("""res8: <error> = """ + $show(res$8));$skip(68); val res$9 = 
  listOf(10, union(choose(-10, -5), choose(5, 10))).sample.run(rng);System.out.println("""res9: <error> = """ + $show(res$9));$skip(105); val res$10 = 
  listOf(100, weighted(choose(-10, -5) -> 0.9, choose(5, 10) -> 0.1)).sample.run(rng)._1.count { _ > 0 };System.out.println("""res10: <error> = """ + $show(res$10));$skip(35); 

  val smallInt = choose(-10, 10);System.out.println("""smallInt  : <error> = """ + $show(smallInt ));$skip(99); 
  val maxProp = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  };System.out.println("""maxProp  : <error> = """ + $show(maxProp ));$skip(15); val res$11 = 
  run(maxProp);System.out.println("""res11: <error> = """ + $show(res$11));$skip(148); val res$12 = 

  run(forAll(listOf1(choose(-100, 100))) { ns =>
    val sorted = ns.sorted
    sorted.head == ns.min && sorted.last == ns.max
  }, 10, 100, rng);System.out.println("""res12: <error> = """ + $show(res$12));$skip(23); val res$13 = 

  run(check(1 == 1));System.out.println("""res13: <error> = """ + $show(res$13));$skip(38); val res$14 = 

  run(forAllBooleans(b => b || !b));System.out.println("""res14: <error> = """ + $show(res$14));$skip(36); val res$15 = 
  run(forAllBooleans(b => b && !b));System.out.println("""res15: <error> = """ + $show(res$15));$skip(252); val res$16 = 
  run(forAll(-1000 to 1000)(n => {
    def sumDigits(m: Int): Int = {
      val sum = m.abs.toString.toList.map(_.toString.toInt).fold(0)(_ + _)
      if (sum.toString.length > 1) sumDigits(sum) else sum
    }
    n == 0 || sumDigits(n * 9) == 9
  }));System.out.println("""res16: <error> = """ + $show(res$16))}
}
