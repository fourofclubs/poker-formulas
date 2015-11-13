package ca.fourofclubs.utils.math

import scala.language.{ implicitConversions, postfixOps }

import Math.gcd

class Rational(a: Int, b: Int) {
  private val v: (Int, Int) = {
    val g = gcd(a, b)
    val n = a / g
    val d = b / g
    if (d > 0) (n, d) else (-n, -d)
  }
  def numer = v _1
  def denom = v _2
  override def toString = numer + (if (denom != 1) "/" + denom else "")
  def unary_- = new Rational(-numer, denom)
  def +(that: Rational) = new Rational(this.numer * that.denom + this.denom * that.numer, this.denom * that.denom)
  def -(that: Rational) = this + -that
  def *(that: Rational) = new Rational(this.numer * that.numer, this.denom * that.denom)
  def inverse = new Rational(denom, numer)
  def /(that: Rational) = this * that.inverse
  def ^(n: Int): Rational = {
    if (n > 0) this * (this ^ (n - 1))
    else if (n < 0) this.inverse ^ -n
    else 1
  }
  def abs = new Rational(a.abs, b.abs)
  def <(that: Rational) = this.numer * that.denom < that.numer * this.denom
  def >(that: Rational) = this.numer * that.denom > that.numer * this.denom
  def <=(that: Rational) = this < that || this == that
  def >=(that: Rational) = this > that || this == that
  override def equals(that: Any) = {
    (if (that.isInstanceOf[Rational]) that.asInstanceOf[Rational]
    else if (that.isInstanceOf[Int]) Rational.int2Rational(that.asInstanceOf[Int])
    else false).toString == this.toString
  }
}

object Rational {
  implicit def int2Rational(x: Int): Rational = new Rational(x, 1)
}