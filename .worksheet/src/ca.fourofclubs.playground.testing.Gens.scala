package ca.fourofclubs.playground.testing
import ca.fourofclubs.playground.testing._
import Gen._
import Prop._
import ca.fourofclubs.playground.random.SimpleRNG
import ca.fourofclubs.playground.par.Par
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

object Gens {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(326); 
  val rng = SimpleRNG(11);System.out.println("""rng  : ca.fourofclubs.playground.random.SimpleRNG = """ + $show(rng ));$skip(121); val res$0 = 
  
  for(f <- intFn[String](choose(-10, 10)).sample.times(10).run(rng)._1) yield (f("Testing"), f("Test"), f("Testing"));System.out.println("""res0: Seq[(Int, Int, Int)] = """ + $show(res$0))}
}
