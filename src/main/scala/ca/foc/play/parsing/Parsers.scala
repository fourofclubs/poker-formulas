package ca.foc.play.parsing

import scala.util.matching.Regex

import ca.foc.play.testing._

trait Parsers[Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  def flatMap[A, B](p: => Parser[A])(f: A => Parser[B]): Parser[B]
  def slice[A](p: Parser[A]): Parser[String]
  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def scope[A](msg: String)(p: Parser[A]): Parser[A]
  def attempt[A](p: Parser[A]): Parser[A]
  def succeed[A](a: A): Parser[A]

  implicit def toLocation(input: String) = Location(input)
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p) { a => succeed(f(a)) }
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = flatMap(p) { a => map(p2) { b => (a, b) } }
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n == 0) succeed(List()) else map2(p, listOfN(n - 1, p))(_ :: _)
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = flatMap(p) { a => map(p2) { b => f(a, b) } }
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] = map2(p, p2) { (_, b) => b }
  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] = map2(p, p2) { (a, _) => a }
  def within[A](left: Parser[Any], right: Parser[Any])(p: => Parser[A]) = left *> p <* right
  def eof: Parser[String] = "\\z".r
  def root[A](p: => Parser[A]) = p <* eof
  def delimited[A](p: Parser[A], p2: => Parser[Any]): Parser[List[A]] = map2(p, many(p2 *> p))(_ :: _)
  def whitespace: Parser[String] = "\\s*".r

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def ^(n: Int) = self.listOfN(n, p)
    def map[B](f: A => B) = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]) = self.flatMap(p)(f)
    def **[B](p2: => Parser[B]) = self.product(p, p2)
    def many = self.many(p)
    def slice = self.slice(p)
    def *>[B](p2: => Parser[B]) = self.skipL(p, p2)
    def <*[B](p2: => Parser[B]) = self.skipR(p, p2)
    def delimitedBy(p2: => Parser[Any]) = self.delimited(p, p2)
    def label(msg: String) = self.label(msg)(p)
    def scope(msg: String) = self.scope(msg)(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)
    def succeedLaw[A](a: A)(in: Gen[String]): Prop = Prop.forAll(in)(s => run(succeed(a))(s) == Right(a))
    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      Prop.forAll(inputs ** Gen.string) {
        case (input, msg) =>
          run(label(msg)(p))(input) match {
            case Left(e) => e.stack.head._2 == msg
            case _       => true
          }
      }
  }
}

case class Location(input: String, offset: Int = 0) {
  lazy val lineNum = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val colNum = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1        => offset + 1
    case lineStart => offset - lineStart
  }
  def toError(msg: String) = ParseError(List((this, msg)))
  def >>(n: Int) = Location(input, offset + n)
  def line = input.slice(0, offset + 1).split("\n").lastOption.getOrElse("")
}
case class ParseError(stack: List[(Location, String)]) {
  def push(loc: Location, msg: String): ParseError = copy(stack = (loc, msg) :: stack)
  def label(loc: Location, msg: String): ParseError = ParseError(latestLoc.map((_, msg)).toList)
  def latestLoc: Option[Location] = latest.map(_._1)
  def latest: Option[(Location, String)] = stack.lastOption

  override def toString = "Parsing Error: " +
    latest.map { case (loc, msg) => s"$msg, at '${loc.line}' (line ${loc.lineNum}, col ${loc.colNum})" }.getOrElse("???")
  def stackTrace = {
    toString :: (for ((loc, msg) <- stack.reverse.tail)
      yield s"$msg, starting at '${loc.line}' (line ${loc.lineNum}, col ${loc.colNum})")
  }.mkString(",\n")
}