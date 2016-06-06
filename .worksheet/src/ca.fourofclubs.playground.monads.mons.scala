package ca.fourofclubs.playground.monads

import ca.fourofclubs.playground.random.RNG
import ca.fourofclubs.playground.random.Rand
import ca.fourofclubs.playground.random.SimpleRNG
import ca.fourofclubs.playground.par._
import ca.fourofclubs.playground.par.Par._
import ca.fourofclubs.playground.monads.Monads._

object mons {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(360); 
  val sm = Monads.stateMonad[RNG];System.out.println("""sm  : ca.fourofclubs.playground.monads.Monad[[x]ca.fourofclubs.playground.State[ca.fourofclubs.playground.random.RNG,x]] = """ + $show(sm ));$skip(22); 
  val ints = Rand.int;System.out.println("""ints  : ca.fourofclubs.playground.random.Rand[Int] = """ + $show(ints ));$skip(34); 
  val rm = sm.replicateM(2, ints);System.out.println("""rm  : ca.fourofclubs.playground.State[ca.fourofclubs.playground.random.RNG,List[Int]] = """ + $show(rm ));$skip(29); 
  val rng = new SimpleRNG(5);System.out.println("""rng  : ca.fourofclubs.playground.random.SimpleRNG = """ + $show(rng ));$skip(14); val res$0 = 
  rm.run(rng);System.out.println("""res0: (List[Int], ca.fourofclubs.playground.random.RNG) = """ + $show(res$0));$skip(33); 

  var p = fork(unit("Testing"));System.out.println("""p  : ca.fourofclubs.playground.par.Par[String] = """ + $show(p ));$skip(23); val res$1 = 
  p.map { _.length() };System.out.println("""res1: ca.fourofclubs.playground.par.Par[Int] = """ + $show(res$1))}
}
