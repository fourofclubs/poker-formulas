package ca.fourofclubs.playground.parsing

import java.util.regex.Pattern

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._
    def bool = ("true" | "TRUE" | "false" | "FALSE") map { b => JBool(b.toBoolean) }
    def number = "[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r map { d => JNumber(d.toDouble) }
    val nullVal = string("null") map { _ => JSON.JNull }
    def token = "\"" *> ((".*?\"").r) map (_.dropRight(1))
    def literal: Parser[JSON] = bool | number | number | nullVal | (token map (JString(_)))
    def array: Parser[JArray] = within("[", "]")(value delimitedBy ",") map (l => JArray(l.toIndexedSeq))
    def keyVal = token ** (":" *> value)
    def obj = within("{", "}")(keyVal.many) map (m => JObject(m.toMap))
    def value: Parser[JSON] = literal | obj | array

    whitespace *> (obj | array)
  }
}