package ca.fourofclubs.playground.parsing

import java.util.regex.Pattern

sealed trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser(P: Parsers[Parser]): Parser[JSON] = {
    import P._
    def bool = ("true" | "TRUE" | "false" | "FALSE") map { b => JBool(b.toBoolean) } label "boolean"
    def number = "[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r map { d => JNumber(d.toDouble) } label "number"
    val nullVal = string("null") map { _ => JSON.JNull } label "null"
    def token = "\"" *> ((".*?\"").r) map (_.dropRight(1)) label "token"
    def literal: Parser[JSON] = (bool | number | nullVal | (token map (JString(_)))) label "literal"
    def array: Parser[JArray] = within("[", "]")((whitespace *> value <* whitespace) delimitedBy ",") map (l => JArray(l.toIndexedSeq)) scope "array"
    def keyVal = token ** (whitespace *> ":" *> whitespace *> value) label "key:value"
    def obj = within("{", "}")((whitespace *> keyVal <* whitespace) delimitedBy ",") map (m => JObject(m.toMap)) scope "object"
    def value: Parser[JSON] = (literal | obj | array) scope "value"

    root(whitespace *> (obj | array) <* whitespace) scope "document"
  }
}