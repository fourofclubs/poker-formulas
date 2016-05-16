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
	val P = ParsersImpl                       //> P  : ca.fourofclubs.playground.parsing.ParsersImpl.type = ca.fourofclubs.pla
                                                  //| yground.parsing.ParsersImpl$@74327432
  val jsonParser = JSON.jsonParser(P)             //> jsonParser  : ca.fourofclubs.playground.parsing.Parser[ca.fourofclubs.playgr
                                                  //| ound.parsing.JSON] = <function1>
	val json = P.run(jsonParser)_             //> json  : String => Either[ca.fourofclubs.playground.parsing.ParseError,ca.fou
                                                  //| rofclubs.playground.parsing.JSON] = <function1>
	json(jsonTxt)                             //> res0: Either[ca.fourofclubs.playground.parsing.ParseError,ca.fourofclubs.pl
                                                  //| ayground.parsing.JSON] = Right(JObject(Map()))
	json(malformedJson1).left                 //> java.util.NoSuchElementException: next on empty iterator
                                                  //| 	at scala.collection.Iterator$$anon$2.next(Iterator.scala:39)
                                                  //| 	at scala.collection.Iterator$$anon$2.next(Iterator.scala:37)
                                                  //| 	at scala.collection.IndexedSeqLike$Elements.next(IndexedSeqLike.scala:63
                                                  //| )
                                                  //| 	at scala.collection.IterableLike$class.head(IterableLike.scala:107)
                                                  //| 	at scala.collection.mutable.ArrayOps$ofRef.scala$collection$IndexedSeqOp
                                                  //| timized$$super$head(ArrayOps.scala:186)
                                                  //| 	at scala.collection.IndexedSeqOptimized$class.head(IndexedSeqOptimized.s
                                                  //| cala:126)
                                                  //| 	at scala.collection.mutable.ArrayOps$ofRef.head(ArrayOps.scala:186)
                                                  //| 	at scala.collection.TraversableLike$class.last(TraversableLike.scala:459
                                                  //| )
                                                  //| 	at scala.collection.mutable.ArrayOps$ofRef.scala$collection$IndexedSeqOp
                                                  //| timized$$super$last(ArrayOps.scala:186)
                                                  //| 	at scala.collection.IndexedSeqOptimized$class.last(IndexedSeqOptimized.s
                                                  //| cala:132)
                                                  //| 	at scala.collection.mutable.Arr
                                                  //| Output exceeds cutoff limit.
	json(malformedJson2)
}