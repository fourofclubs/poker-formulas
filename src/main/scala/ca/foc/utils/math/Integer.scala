package ca.foc.utils.math
import Nat._

class Integer(n: Nat, pos: Boolean) {
  override def toString = toStringInBase(Ten)
  def toStringInBase(base: Nat) = (if (!pos) "-" else "") + n.toStringInBase(base)
  def toBinaryString = toStringInBase(Two)
  def toOctalString = toStringInBase(Eight)
  def toHexString = toStringInBase(Eight * Two)
  def unary_- = new Integer(n, !pos)
}

object Integer {
  def apply(str: String, base: Nat): Integer = str.toList match {
    case List()    ⇒ new Integer(Zero, true)
    case '-' :: cs ⇒ -Integer(cs mkString, base)
    case _         ⇒ new Integer(Nat(str, base), true)
  }
  def apply(str: String): Integer = str.toList match {
    case '-' :: cs ⇒ -Integer(cs mkString)
    case 'b' :: cs ⇒ Integer(cs mkString, Two)
    case 'h' :: cs ⇒ Integer(cs mkString, Eight * Two)
    case _         ⇒ Integer(str, Ten)
  }
}