package ca.fourofclubs.playground

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable
import java.util.concurrent.Executors
import ca.fourofclubs.playground.par._

object Pars {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(324); 
  val es = Executors.newCachedThreadPool();System.out.println("""es  : java.util.concurrent.ExecutorService = """ + $show(es ));$skip(66); val res$0 = 
  choiceN(unit(2))(List(unit("a"), unit("b"), unit("c")))(es).get;System.out.println("""res0: String = """ + $show(res$0));$skip(48); val res$1 = 
  choice(unit(false))(unit("T"), unit("F"))(es);System.out.println("""res1: java.util.concurrent.Future[String] = """ + $show(res$1));$skip(37); 

  val aF = asyncF[Int, Int](_ + 4);System.out.println("""aF  : Int => ca.fourofclubs.playground.par.Par[Int] = """ + $show(aF ));$skip(16); val res$2 = 
  aF(5)(es).get;System.out.println("""res2: Int = """ + $show(res$2));$skip(18); val res$3 = 
  unit(5)(es).get;System.out.println("""res3: Int = """ + $show(res$3));$skip(47); val res$4 = 
  sequence(List(aF(5), aF(4), aF(10)))(es).get;System.out.println("""res4: List[Int] = """ + $show(res$4));$skip(64); val res$5 = 
  parFilter(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(_ < 5)(es).get;System.out.println("""res5: List[Int] = """ + $show(res$5));$skip(16); 
  es.shutdown()}
}
