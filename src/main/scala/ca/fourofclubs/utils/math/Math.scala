package ca.fourofclubs.utils.math

object Math {
  def gcd(x: Int, y: Int): Int = if (x == 0) y else gcd(y % x, x)
}