package ca.foc.mem
import PokerFormulas._

object memdeck {
  def acaan(c: Card, n: Int): Card = {
    val m = MemDeck.positionOf(c)
    val cut = ((m - n + 51) % 51) + 1
    MemDeck.cut(cut).bottom
  }                                               //> acaan: (c: ca.foc.mem.Card, n: Int)ca.foc.mem.Card
	HANDS                                     //> res0: Set[ca.foc.mem.PokerFormulas.Hand] = null
  (for (
    players ← (3 to 10);
    h ← HANDS;
    d ← findHand(MemDeck, h, players)
  ) yield HandInfo(h, players) -> d).toMap        //> java.lang.NullPointerException
                                                  //| 	at ca.foc.mem.memdeck$$anonfun$main$1$$anonfun$1.apply(ca.foc.mem.memdec
                                                  //| k.scala:13)
                                                  //| 	at ca.foc.mem.memdeck$$anonfun$main$1$$anonfun$1.apply(ca.foc.mem.memdec
                                                  //| k.scala:12)
                                                  //| 	at scala.collection.TraversableLike$$anonfun$flatMap$1.apply(Traversable
                                                  //| Like.scala:252)
                                                  //| 	at scala.collection.TraversableLike$$anonfun$flatMap$1.apply(Traversable
                                                  //| Like.scala:252)
                                                  //| 	at scala.collection.immutable.Range.foreach(Range.scala:166)
                                                  //| 	at scala.collection.TraversableLike$class.flatMap(TraversableLike.scala:
                                                  //| 252)
                                                  //| 	at scala.collection.AbstractTraversable.flatMap(Traversable.scala:104)
                                                  //| 	at ca.foc.mem.memdeck$$anonfun$main$1.apply$mcV$sp(ca.foc.mem.memdeck.sc
                                                  //| ala:12)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.work
                                                  //| Output exceeds cutoff limit.

  val h = HandInfo(Flush(S, v9), 3)
  seconds.get(h)

  for (v ← sortedValues) yield {
    val h = HandInfo(Flush(S, v), 3)
    seconds.get(h).map(fmt(h)_).getOrElse("")
  }
  val flushes = {
    for (s ← SUITS) yield {
      for (p ← 3 to 10) yield {
        for (v ← sortedValues) yield {
          val h = HandInfo(Flush(s, v), p)
          seconds.get(h).map(fmt(h)_).getOrElse("")
        }
      }.mkString("\t")
    }.mkString("\n")
  }.mkString("\n\n")

}