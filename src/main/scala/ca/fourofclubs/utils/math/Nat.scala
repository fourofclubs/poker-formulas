package ca.fourofclubs.utils.math

abstract class Nat extends Ordered[Nat] {
  def isZero: Boolean
  def successor: Nat = new Successor(this)
  def precessor: Nat
  def +(that: Nat): Nat = if (that.isZero) this else this.successor + that.precessor
  def -(that: Nat): Nat = if (that.isZero) this else this.successor - that.precessor
  def *(that: Nat): Nat = if (that.isZero) that else that.precessor * this + this
  def ==(that: Nat): Boolean
  def compare(that: Nat): Int
}

object Zero extends Nat {
  override def toString = "0"
  def isZero = true
  def precessor = throw new IllegalStateException("Negative natural number.")
  def ==(that: Nat) = that.isZero
  def compare(that: Nat) = if (that.isZero) 0 else -1
}

class Successor(n: Nat) extends Nat {
  override def toString = n + "+1"
  def isZero = false
  def precessor = n
  def ==(that: Nat) = !that.isZero && this.precessor == that.precessor
  def compare(that: Nat) = if (that.isZero) 1 else this.precessor.compare(that.precessor)
}