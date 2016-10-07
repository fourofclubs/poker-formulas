package ca.fourofclubs.utils.collections

import com.google.common.collect.Ordering
import com.google.common.collect.Lists
import com.google.common.base.Functions
import ca.foc.utils.collections.LinkedList
import com.google.common.primitives.Ints
import com.google.common.collect.Iterables
import scala.collection.JavaConversions._
import java.util.Random
import org.joda.time.DateTime

object scratch {
  val list = "testing" :: "something" :: "Test" :: Nil
                                                  //> list  : List[String] = List(testing, something, Test)
  ("Test" :: list).size                           //> res0: Int = 4
  ("Test" :: list)(2)                             //> res1: String = something

  4 :: Nil                                        //> res2: List[Int] = List(4)

  Iterable(3, 2, 4)                               //> res3: Iterable[Int] = List(3, 2, 4)
  LinkedList(Iterable(4, 2, 3))                   //> res4: ca.foc.utils.collections.LinkedList[Any] = [4,[2,[3,Nil]]]

  val es = Lists.newArrayList[Object](Integer.valueOf(2), Integer.valueOf(1), java.lang.Double.valueOf(1.24), "012", null)
                                                  //> es  : java.util.ArrayList[Object] = [2, 1, 1.24, 012, null]

  Ordering.natural[java.lang.String]().nullsFirst[java.lang.String]().onResultOf[Object](Functions.constant(null)).
    nullsLast[Object]().sortedCopy(es)            //> res5: java.util.List[Object] = [2, 1, 1.24, 012, null]

  Ints.tryParse("")                               //> res6: Integer = null
  Iterables.getFirst(List(), null)                //> res7: Null = null

  val r = new Random                              //> r  : java.util.Random = java.util.Random@61596159
  Long.MaxValue                                   //> res8: Long(9223372036854775807L) = 9223372036854775807
  Long.MinValue                                   //> res9: Long(-9223372036854775808L) = -9223372036854775808
  def t(l: Long) = (l / 10000000) + 100000000000L //> t: (l: Long)Long
  def toDate(l: Long) = {
    val m = (l / 10000000) + 2000000000000L
    println(m)
    new DateTime(m)
  }                                               //> toDate: (l: Long)org.joda.time.DateTime
  t(Long.MinValue)                                //> res10: Long = -822337203685
  toDate(Long.MinValue)                           //> 1077662796315
                                                  //| res11: org.joda.time.DateTime = 2004-02-24T16:46:36.315-06:00
  toDate(Long.MaxValue)                           //> 2922337203685
                                                  //| res12: org.joda.time.DateTime = 2062-08-09T03:20:03.685-05:00

}