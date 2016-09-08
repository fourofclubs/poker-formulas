package ca.foc.utils.math

import scala.language.{ implicitConversions, postfixOps }

import Nat.{ Eight, Two }

abstract class Nat {
  def isZero: Boolean
  def successor: Nat = new Successor(this)
  def precessor: Nat
  def +(that: Nat): Nat = if (that.isZero) this else this.successor + that.precessor
  def -(that: Nat): Nat = if (that.isZero) this else this.precessor - that.precessor
  def *(that: Nat): Nat = if (that.isZero) that else that.precessor * this + this
  def <(that: Nat): Boolean
  def <=(that: Nat) = this == that || this < that
  def >(that: Nat) = !(this <= that)
  def >=(that: Nat) = !(this < that)
  def /(that: Nat): (Nat, Nat) =
    if (that.isZero) throw new IllegalArgumentException("Cannot divide by zero.")
    else if (this < that) (Zero, this)
    else ((this - that) / that) match { case (q, r) ⇒ (q.successor, r) }
  def toStringInBase(base: Nat) = Nat.toBase(this, base)
  def toBinaryString = toStringInBase(Two)
  def toOctalString = toStringInBase(Eight)
  def toHexString = toStringInBase(Eight * Two)
}

object Zero extends Nat {
  override def toString = Nat.digit(Zero).toString
  def isZero = true
  def precessor = throw new IllegalStateException("Negative natural number.")
  override def equals(that: Any) = that.isInstanceOf[Nat] && that.asInstanceOf[Nat].isZero
  def <(that: Nat) = !that.isZero
}

class Successor(n: Nat) extends Nat {
  override def toString = this.toStringInBase(Nat.Ten)
  def isZero = false
  def precessor = n
  override def equals(that: Any) = that.isInstanceOf[Nat] && !that.asInstanceOf[Nat].isZero &&
    this.precessor == that.asInstanceOf[Nat].precessor
  def <(that: Nat) = !that.isZero && this.precessor < that.precessor
}

object Nat {
  val One = Zero.successor
  val Two = One.successor
  val Three = Two.successor
  val Four = Three.successor
  val Five = Four.successor
  val Six = Five.successor
  val Seven = Six.successor
  val Eight = Seven.successor
  val Nine = Eight.successor
  val Ten = Nine.successor
  private val digits = List('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E',
    'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')
  private[math] def digit(n: Nat) = getAt(n, digits)
  private def getAt[T](n: Nat, xs: List[T]): T = if (n.isZero) xs.head else getAt(n - One, xs.tail)
  private def indexOf[T](x: T, xs: List[T]): Nat = if (xs.head == x) Zero else indexOf(x, xs.tail).successor
  private[math] def toBase(n: Nat, base: Nat): String = {
    if (base < Two) throw new IllegalArgumentException("Base must be at least two.")
    n / base match { case (q, r) ⇒ (if (q.isZero) "" else toBase(q, base)) + digit(r) }
  }
  def apply(str: String, base: Nat): Nat = str.toList match {
    case List()  ⇒ Zero
    case cs :+ c ⇒ (Nat(cs mkString, base) * base) + indexOf(c, digits)
  }
  def apply(str: String): Nat = str.toList match {
    case 'b' :: cs ⇒ Nat(cs mkString, Two)
    case 'h' :: cs ⇒ Nat(cs mkString, Eight * Two)
    case _         ⇒ Nat(str, Ten)
  }
  implicit def string2Nat(str: String) = Nat(str)
}