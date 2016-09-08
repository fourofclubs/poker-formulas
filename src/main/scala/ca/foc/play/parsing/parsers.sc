package ca.fourofclubs.playground.parsing

import ca.fourofclubs.playground.parsing.JSON.JNumber
import ca.fourofclubs.playground.testing.Gen
import ca.fourofclubs.playground.random.SimpleRNG
import ca.fourofclubs.playground.testing.SGen
import ca.fourofclubs.playground.parsing.JSON.JBool
import ca.fourofclubs.playground.parsing.JSON.JString
import ca.fourofclubs.playground.parsing.JSON.JArray
import ca.fourofclubs.playground.parsing.JSON.JObject
import ca.fourofclubs.playground.parsing.JSON.JArray
import ca.fourofclubs.playground.parsing.JSON.JObject
import ca.fourofclubs.playground.parsing.JSON.JArray
import scala.util.Try
import com.ibm.rmi.util.Store.NoSuchElementException

object parsing {
  val jsonTxt = """
{
}
"""                                               //> jsonTxt  : String = "
                                                  //| {
                                                  //| }
                                                  //| "

  val malformedJson1 = """
{
  "Company name" ; "Microsoft Corporation"
}
"""                                               //> malformedJson1  : String = "
                                                  //| {
                                                  //|   "Company name" ; "Microsoft Corporation"
                                                  //| }
                                                  //| "

  val malformedJson2 = """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
"""                                               //> malformedJson2  : String = "
                                                  //| [
                                                  //|   [ "HPQ", "IBM",
                                                  //|   "YHOO", "DELL" ++
                                                  //|   "GOOG"
                                                  //|   ]
                                                  //| ]
                                                  //| "
  val P = ParsersImpl                             //> P  : ca.fourofclubs.playground.parsing.ParsersImpl.type = ca.fourofclubs.pla
                                                  //| yground.parsing.ParsersImpl$@76cb76cb
  val jsonParser = JSON.jsonParser(P)             //> jsonParser  : ca.fourofclubs.playground.parsing.Parser[ca.fourofclubs.playgr
                                                  //| ound.parsing.JSON] = <function1>
  val json = P.run(jsonParser)_                   //> json  : String => Either[ca.fourofclubs.playground.parsing.ParseError,ca.fou
                                                  //| rofclubs.playground.parsing.JSON] = <function1>
  json(jsonTxt)                                   //> res0: Either[ca.fourofclubs.playground.parsing.ParseError,ca.fourofclubs.pl
                                                  //| ayground.parsing.JSON] = Left(Parsing Error: Expected: 'key:value', at '}' 
                                                  //| (line 3, col 1))
  json("{\"jurisdictionId\":3,\"formId\":11}")    //> res1: Either[ca.fourofclubs.playground.parsing.ParseError,ca.fourofclubs.pl
                                                  //| ayground.parsing.JSON] = Right(JObject(Map(jurisdictionId -> JNumber(3.0), 
                                                  //| formId -> JNumber(11.0))))\
}