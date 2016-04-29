package ca.fourofclubs.playground.testing
import ca.fourofclubs.playground.testing._
import Gen._
import Prop._
import ca.fourofclubs.playground.random.SimpleRNG
import ca.fourofclubs.playground.par.Par
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

object Gens {
  val rng = SimpleRNG(11)                         //> rng  : ca.fourofclubs.playground.random.SimpleRNG = SimpleRNG(11)
  
  for(f <- intFn[String](choose(-10, 10)).sample.times(10).run(rng)._1) yield (f("Testing"), f("Test"), f("Testing"))
                                                  //> res0: Seq[(Int, Int, Int)] = List((946121,10168,946121), (121103608,1301593,
                                                  //| 121103608), (30275902,325398,30275902), (30275902,325398,30275902), (3027590
                                                  //| 2,325398,30275902), (473060,5084,473060), (7,0,7), (30275902,325398,30275902
                                                  //| ), (15137951,162699,15137951), (0,0,0))
}