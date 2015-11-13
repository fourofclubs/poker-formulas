package ca.fourofclubs.utils.math
import Complex._
import Math._
import Rational._

object scratch {
  val e1 = Prod(Sum(Val(1), Sum(Var("x"), Val(8))), Sum(Var("y"), Val(5)))
                                                  //> e1  : ca.fourofclubs.utils.math.Prod = (1+x+8)*(y+5)
  e1.eval("x" -> 3, "y" -> 2)                     //> res0: Double = 84.0
}