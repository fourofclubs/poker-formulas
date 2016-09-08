package ca.fourofclubs.playground.par

import java.util.concurrent.Executors
import ca.fourofclubs.playground.par.Par.choice
import ca.fourofclubs.playground.par.Par.unit

object Pars {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(228); 
  val es = Executors.newCachedThreadPool();System.out.println("""es  : java.util.concurrent.ExecutorService = """ + $show(es ));$skip(117); val res$0 = 
  // choiceN(unit(2))(List(unit("a"), unit("b"), unit("c")))(es).get

	choice(unit(false))(unit("T"), unit("F"))(es);System.out.println("""res0: java.util.concurrent.Future[String] = """ + $show(res$0));$skip(267); 
  //choice(unit(false))(unit("T"), unit("F"))(es)

  //  val aF = asyncF[Int, Int](_ + 4)
  //  aF(5)(es).get
  //  unit(5)(es).get
  //  sequence(List(aF(5), aF(4), aF(10)))(es).get
  //  parFilter(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(_ < 5)(es).get
  es.shutdown()}
}
