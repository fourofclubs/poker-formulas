package ca.foc.play

package object parsing {
  type Parser[+A] = Location => Result[A]

  sealed trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _             => this
    }
    def addCommit(committed: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || committed)
      case _             => this
    }
    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _                => this
    }
    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n + m)
      case _             => this
    }
  }
  case class Success[+A](get: A, consumed: Int) extends Result[A]
  case class Failure(get: ParseError, committed: Boolean) extends Result[Nothing] {
    override def toString = "Parse Error: " + get
  }
}