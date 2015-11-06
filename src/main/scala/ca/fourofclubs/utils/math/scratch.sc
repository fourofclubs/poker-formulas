package ca.fourofclubs.utils.math
import Complex._
import Math._
import Rational._

object scratch {
  Complex(1, 2)                                   //> res0: ca.fourofclubs.utils.math.Complex = 1+2i
  val one = Zero.successor                        //> one  : ca.fourofclubs.utils.math.Nat = 0+1
  val two = one + one                             //> two  : ca.fourofclubs.utils.math.Nat = 0+1+1
  val three = two + one                           //> three  : ca.fourofclubs.utils.math.Nat = 0+1+1+1
  Zero * two                                      //> res1: ca.fourofclubs.utils.math.Nat = 0
  two * two + one                                 //> res2: ca.fourofclubs.utils.math.Nat = 0+1+1+1+1+1
  two == one                                      //> res3: Boolean = false
  one == one                                      //> res4: Boolean = true
  one == Zero.successor                           //> res5: Boolean = true
  one < Zero                                      //> res6: Boolean = false
  one > Zero                                      //> res7: Boolean = true
  two >= two                                      //> res8: Boolean = true
  one <= two                                      //> res9: Boolean = true
}