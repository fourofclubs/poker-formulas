package ca.fourofclubs.playground.monoids

import ca.fourofclubs.playground.testing.Gen
import ca.fourofclubs.playground.random.SimpleRNG
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

object mon {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(258); 
  val rng = new SimpleRNG(10);System.out.println("""rng  : ca.fourofclubs.playground.random.SimpleRNG = """ + $show(rng ));$skip(41); 
  val intGen = Gen.choose(-10000, 10000);System.out.println("""intGen  : ca.fourofclubs.playground.testing.Gen[Int] = """ + $show(intGen ));$skip(71); val res$0 = 
  Monoids.monoidLaws(Monoids.intAddition, intGen).run(1000, 1000, rng);System.out.println("""res0: ca.fourofclubs.playground.testing.Result = """ + $show(res$0));$skip(77); val res$1 = 
  Monoids.monoidLaws(Monoids.intMultiplication, intGen).run(1000, 1000, rng);System.out.println("""res1: ca.fourofclubs.playground.testing.Result = """ + $show(res$1));$skip(74); val res$2 = 
  Monoids.monoidLaws(Monoids.booleanOr, Gen.boolean).run(1000, 1000, rng);System.out.println("""res2: ca.fourofclubs.playground.testing.Result = """ + $show(res$2));$skip(75); val res$3 = 
  Monoids.monoidLaws(Monoids.booleanAnd, Gen.boolean).run(1000, 1000, rng);System.out.println("""res3: ca.fourofclubs.playground.testing.Result = """ + $show(res$3));$skip(90); 

  val intOptionGen = Gen.weighted(Gen.unit(None) -> 0.1, intGen.map { Some(_) } -> 0.9);System.out.println("""intOptionGen  : ca.fourofclubs.playground.testing.Gen[Option[Int]] = """ + $show(intOptionGen ));$skip(83); val res$4 = 
  Monoids.monoidLaws(Monoids.optionMonoid[Int], intOptionGen).run(1000, 1000, rng);System.out.println("""res4: ca.fourofclubs.playground.testing.Result = """ + $show(res$4));$skip(79); val res$5 = 

 Monoids.foldMapV(IndexedSeq(1, 2, 3, 4, 5, 6), Monoids.intAddition)(a => a);System.out.println("""res5: Int = """ + $show(res$5));$skip(43); 
  val es = Executors.newCachedThreadPool();System.out.println("""es  : java.util.concurrent.ExecutorService = """ + $show(es ));$skip(90); val res$6 = 
  Monoids.parFoldMap(IndexedSeq(1, 2, 3, 4, 5, 6), Monoids.intMultiplication)(a => a)(es);System.out.println("""res6: java.util.concurrent.Future[Int] = """ + $show(res$6));$skip(49); val res$7 = 
	Monoids.wordCount("lorem ipsum dolor sit amet");System.out.println("""res7: Int = """ + $show(res$7));$skip(41); val res$8 = 
	Monoids.bag(IndexedSeq(0, 0, 1, 0, 10));System.out.println("""res8: Map[Int,Int] = """ + $show(res$8));$skip(66); 
                                                  
  es.shutdown;$skip(44); val res$9 = 
  es.awaitTermination(10, TimeUnit.SECONDS);System.out.println("""res9: Boolean = """ + $show(res$9))}
}
