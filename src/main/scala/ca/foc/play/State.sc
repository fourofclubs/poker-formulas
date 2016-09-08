package ca.fourofclubs.playground

import scala.annotation.tailrec
import com.ibm.gsk.ikeyman.util.StateMachineFactory
import playground._

object States {
  val rng = SimpleRNG(3)                          //> rng  : ca.fourofclubs.playground.SimpleRNG = SimpleRNG(3)
  doubleInt.run(rng)._1                           //> res0: (Double, Int) = (5.37487678480096E-4,832745806)

  nonNegativeLessThan(6).run(rng)._1              //> res1: Int = 2
  int.times(10).run(rng)._1                       //> res2: List[Int] = List(1154246, 832745806, -2005759122, -973416119, -1487304
                                                  //| 294, -387508411, -1093251575, -594271733, 1787214961, -1431003963)

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

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
  })                                              //> simulateMachine: (inputs: List[ca.fourofclubs.playground.States.Input])ca.f
                                                  //| ourofclubs.playground.State[ca.fourofclubs.playground.States.Machine,(Int, 
                                                  //| Int)]

  simulateMachine(List(Coin, Turn)).run(Machine(true, 10, 2))
                                                  //> res3: ((Int, Int), ca.fourofclubs.playground.States.Machine) = ((3,9),Machi
                                                  //| ne(true,9,3))
}