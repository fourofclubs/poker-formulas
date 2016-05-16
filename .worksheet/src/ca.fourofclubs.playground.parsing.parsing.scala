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

object parsing {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(733); 
  val jsonTxt = """
{
}
""";System.out.println("""jsonTxt  : String = """ + $show(jsonTxt ));$skip(79); 

  val malformedJson1 = """
{
  "Company name" ; "Microsoft Corporation"
}
""";System.out.println("""malformedJson1  : String = """ + $show(malformedJson1 ));$skip(87); 

  val malformedJson2 = """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
""";System.out.println("""malformedJson2  : String = """ + $show(malformedJson2 ));$skip(21); 
	val P = ParsersImpl;System.out.println("""P  : ca.fourofclubs.playground.parsing.ParsersImpl.type = """ + $show(P ));$skip(38); 
  val jsonParser = JSON.jsonParser(P);System.out.println("""jsonParser  : ca.fourofclubs.playground.parsing.Parser[ca.fourofclubs.playground.parsing.JSON] = """ + $show(jsonParser ));$skip(31); 
	val json = P.run(jsonParser)_;System.out.println("""json  : String => Either[ca.fourofclubs.playground.parsing.ParseError,ca.fourofclubs.playground.parsing.JSON] = """ + $show(json ));$skip(15); val res$0 = 
	json(jsonTxt);System.out.println("""res0: Either[ca.fourofclubs.playground.parsing.ParseError,ca.fourofclubs.playground.parsing.JSON] = """ + $show(res$0));$skip(27); val res$1 = 
	json(malformedJson1).left;System.out.println("""res1: scala.util.Either.LeftProjection[ca.fourofclubs.playground.parsing.ParseError,ca.fourofclubs.playground.parsing.JSON] = """ + $show(res$1));$skip(22); val res$2 = 
	json(malformedJson2);System.out.println("""res2: Either[ca.fourofclubs.playground.parsing.ParseError,ca.fourofclubs.playground.parsing.JSON] = """ + $show(res$2))}
}
