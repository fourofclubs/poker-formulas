package ca.fourofclubs.utils.math

import java.lang.Math.sqrt

import scala.BigDecimal
import scala.language.implicitConversions
import scala.math.BigDecimal.int2bigDecimal

/** A complex number
  * @param a The real part.
  * @param b The imaginary part.
  */
final class Complex(a: BigDecimal, b: BigDecimal) {
  /** Gets the real part of the complex value */
  def re = a
  /** Gets the imaginary part of the complex value */
  def im = b
  /** Adds this to the given complex value */
  def +(that: Complex) = new Complex(this.re + that.re, this.im + that.im)
  /** Negation */
  def unary_- = new Complex(-a, -b)
  /** Subtracts the given complex value from this */
  def -(that: Complex) = this + -that
  /** Multiplies this with the given complex value */
  def *(that: Complex) = new Complex(this.re * that.re - this.im * that.im, this.re * that.im + this.im * that.re)
  /** Complex conjugation */
  def unary_~ = new Complex(this.re, -this.im) //complex conjugation
  /** Absolute value */
  def abs = Complex.abs(this)
  /** The multiplicative inverse */
  def inverse = {
    val denom = this.re * this.re + this.im * this.im
    new Complex(this.re / denom, -this.im / denom)
  }
  /** Divides this by the given complex value */
  def /(that: Complex) = this * that.inverse
  /** Exponentiation */
  def ^(n: Int): Complex = {
    if (n > 0) this * (this ^ (n - 1))
    else if (n < 0) (-this) ^ n
    else Complex.ONE
  }
  override def toString = {
    def aString = if (a != 0) a else ""
    def bString = if (b == 0) "" else if (b.abs != 1) b.abs + "i" else "i"
    def sign = if (b == 0) "" else if (b < 0) "-" else if (a != 0) "+" else ""
    aString + sign + bString
  }
}

object Complex {
  /** Absolute value. */
  def abs(x: Complex) = sqrt((x.re * x.re + x.im * x.im).toDouble)
  val ONE = new Complex(1, 0)
  val ZERO = new Complex(0, 0)
  val i = new Complex(0, 1)
  def apply(a: BigDecimal, b: BigDecimal) = new Complex(a, b)
  implicit def bigDecimal2Complex(x: BigDecimal) = new Complex(x, 0)
  implicit def double2Complex(x: Double) = new Complex(BigDecimal.apply(x), 0)
}