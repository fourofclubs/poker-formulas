package ca.fourofclubs.utils.math
import Nat._

object scratch {
  Nat("10") * "11"                                //> res0: ca.fourofclubs.utils.math.Nat = 110

  string2Nat("hA")                                //> res1: ca.fourofclubs.utils.math.Nat = 10
  Spring - 120                                    //> res2: <error> = Spring
}

sealed trait Season {
  def +(n: Int): Season = if (n > 0) next + (n - 1) else if (n < 0) previous + (n + 1) else this
  def -(n: Int) = this + (-n)
  def next: Season
  def previous: Season
}
case object Spring extends Season { def next = Summer; def previous = Winter }
case object Summer extends Season { def next = Autumn; def previous = Spring }
case object Autumn extends Season { def next = Winter; def previous = Summer }
case object Winter extends Season { def next = Spring; def previous = Autumn }