package ca.foc.utils.math

sealed trait Expr {
  override def toString = this match {
    case Val(n)        ⇒ n.toString
    case Var(name)     ⇒ name
    case Sum(e1, e2)   ⇒ binaryOpString(e1, e2, "+")
    case Minus(e1, e2) ⇒ binaryOpString(e1, e2, "-")
    case Prod(e1, e2)  ⇒ binaryOpString(e1, e2, "*")
    case Div(e1, e2)   ⇒ binaryOpString(e1, e2, "/")
    case Neg(e)        ⇒ "-(" + e + ")"
    case Exp(e, n) ⇒ e match {
      case Val(_) | Var(_) ⇒ e.toString + "^" + n
      case _               ⇒ "(" + e + ")^" + n
    }
  }
  def eval(vars: (String, Int)*): Double = eval((vars :\ Map[String, Int]())((entry, map) ⇒ map + entry))
  def eval(vars: Map[String, Int]): Double = this match {
    case Val(n)        ⇒ n
    case Var(x)        ⇒ vars(x)
    case Neg(e)        ⇒ -e.eval(vars)
    case Sum(e1, e2)   ⇒ e1.eval(vars) + e2.eval(vars)
    case Minus(e1, e2) ⇒ e1.eval(vars) - e2.eval(vars)
    case Prod(e1, e2)  ⇒ e1.eval(vars) * e2.eval(vars)
    case Div(e1, e2)   ⇒ e1.eval(vars) / e2.eval(vars)
    case Exp(e, n)     ⇒ if (n >= 0) exp(e.eval(vars), n) else Exp(Val(1) / e, -n).eval(vars)
  }
  private def exp(r: Double, n: Int): Double = if (n == 0) 1 else r * exp(r, n - 1)
  def simplify: Expr = this match {
    case Val(n) ⇒ if (n < 0) Neg(Val(-n)) else this
    case Var(_) ⇒ this
    case Neg(e) ⇒ e match {
      case Val(n)      ⇒ if (n < 0) Val(-n) else this
      case Neg(e1)     ⇒ e1.simplify
      case Div(e1, e2) ⇒ Div(Neg(e1.simplify), e2.simplify)
      case _           ⇒ Neg(e.simplify)
    }
    case Sum(e1, e2) ⇒ e2 match {
      case Neg(e) ⇒ Minus(e1.simplify, e.simplify).simplify
      case _      ⇒ Sum(e1.simplify, e2.simplify)
    }
    case Minus(e1, e2) ⇒ e2 match {
      case Neg(e) ⇒ Sum(e1.simplify, e.simplify)
      case _      ⇒ Minus(e1.simplify, e2.simplify)
    }
    case _ ⇒ this
  }
  def +(that: Expr) = Sum(this, that)
  def *(that: Expr) = Prod(this, that)
  def /(that: Expr) = Div(this, that)
  def -(that: Expr) = Minus(this, that)
  def unary_- = Neg(this)
  def ^(n: Int) = Exp(this, n)
  private def binaryOpString(e1: Expr, e2: Expr, op: String): String = {
    def eStr(e: Expr) = if (e.precedence > this.precedence) "(" + e + ")" else e.toString
    eStr(e1) + op + eStr(e2)
  }
  private def precedence: Int = this match {
    case Val(_) | Var(_)         ⇒ 0
    case Neg(_)                  ⇒ 1
    case Exp(_, _)               ⇒ 2
    case Prod(_, _) | Div(_, _)  ⇒ 3
    case Sum(_, _) | Minus(_, _) ⇒ 4
  }
}
case class Val(n: Int) extends Expr { assert(n > 0, "Value must be positive.") }
case class Var(name: String) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Minus(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
case class Div(e1: Expr, e2: Expr) extends Expr
case class Neg(e: Expr) extends Expr
case class Exp(e: Expr, n: Int) extends Expr