package ca.fourofclubs.playground

import scala.annotation.tailrec
import com.ibm.gsk.ikeyman.util.StateMachineFactory
import playground._

object States {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(182); 
  val rng = SimpleRNG(3);System.out.println("""rng  : ca.fourofclubs.playground.SimpleRNG = """ + $show(rng ));$skip(24); val res$0 = 
  doubleInt.run(rng)._1;System.out.println("""res0: (Double, Int) = """ + $show(res$0));$skip(39); val res$1 = 

  nonNegativeLessThan(6).run(rng)._1;System.out.println("""res1: Int = """ + $show(res$1));$skip(28); val res$2 = 
  int.times(10).run(rng)._1

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int);System.out.println("""res2: List[Int] = """ + $show(res$2));$skip(804); 

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(m => {
  	val unchanged = ((m.coins, m.candies), m)
    def takeInput(input: Input): State[Machine, (Int, Int)] = State(m => {
      if (m.candies <= 0) unchanged
      else input match {
        case Coin => if (m.locked) ((m.coins + 1, m.candies), Machine(false, m.candies, m.coins + 1)) else unchanged
        case Turn => if (m.locked) unchanged else ((m.coins, m.candies - 1), Machine(true, m.candies - 1, m.coins))
      }
    })
    inputs match {
      case List()  => unchanged
      case i :: is => simulateMachine(is).run(takeInput(i).run(m)._2)
    }
  });System.out.println("""simulateMachine: (inputs: List[ca.fourofclubs.playground.States.Input])ca.fourofclubs.playground.State[ca.fourofclubs.playground.States.Machine,(Int, Int)]""");$skip(64); val res$3 = 

  simulateMachine(List(Coin, Turn)).run(Machine(true, 10, 2));System.out.println("""res3: ((Int, Int), ca.fourofclubs.playground.States.Machine) = """ + $show(res$3))}
}
