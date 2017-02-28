package ca.foc.play.parsing

import scala.Left
import scala.Right
import scala.language.implicitConversions
import scala.util.matching.Regex

object ParsersImpl extends Parsers[Parser] {
  def flatMap[A, B](p: => Parser[A])(f: A ⇒ Parser[B]): Parser[B] = loc => p(loc) match {
    case Success(a, n)     => f(a)(loc >> n).addCommit(n != 0).advanceSuccess(n)
    case e @ Failure(_, _) => e
  }
  def or[A](s1: Parser[A], s2: ⇒ Parser[A]): Parser[A] = loc => s1(loc) match {
    case Failure(e, false) => s2(loc)
    case r                 => r
  }
  def attempt[A](p: Parser[A]): Parser[A] = loc => p(loc).uncommit
  def label[A](msg: String)(p: Parser[A]): Parser[A] = loc => p(loc).mapError(_.label(loc, s"Expected: '$msg'"))
  def succeed[A](a: A): Parser[A] = loc => Success(a, 0)
  def scope[A](scope: String)(p: Parser[A]): Parser[A] = loc => p(loc).mapError(_.push(loc, s"In Scope:'$scope'"))
  def slice[A](p: Parser[A]): Parser[String] = loc => p(loc) match {
    case Success(_, n) => Success(loc.input.slice(loc.offset, loc.offset + n), n)
    case f: Failure    => f
  }
  implicit def regex(r: Regex): Parser[String] = (loc: Location) => r.findPrefixOf(input(loc))
    .map(s => Success(s, s.length)).getOrElse(Failure(loc toError ("Expected: " + r), false))
  implicit def string(s: String): Parser[String] = (loc: Location) =>
    if (input(loc).startsWith(s)) Success(s, s.length)
    else Failure(loc toError ("Expected: " + s), false)
  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = p(Location(input)) match {
    case Success(a, _) => Right(a)
    case Failure(e, _) => Left(e)
  }
  private def input(loc: Location) = loc.input.drop(loc.offset)
}