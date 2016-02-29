package ca.fourofclubs.utils.collections

import com.google.common.collect.Ordering
import com.google.common.collect.Lists
import com.google.common.base.Functions

object scratch {
  val list = "testing" :: "something" :: "Test" :: Nil
                                                  //> list  : ca.fourofclubs.utils.collections.LinkedList[Any] = [testing,[somethi
                                                  //| ng,[Test,Nil]]]
  ("Test" :: list).size                           //> res0: Int = 4
  ("Test" :: list)(2)                             //> res1: Any = something

  4 :: Nil                                        //> res2: ca.fourofclubs.utils.collections.LinkedList[Any] = [4,Nil]
  
  

  Iterable(3, 2, 4)                               //> res3: Iterable[Int] = List(3, 2, 4)
  LinkedList(Iterable(4, 2, 3))                   //> res4: ca.fourofclubs.utils.collections.LinkedList[Any] = [4,[2,[3,Nil]]]

  val es = Lists.newArrayList[Object](Integer.valueOf(2), Integer.valueOf(1), java.lang.Double.valueOf(1.24), "012", null)
                                                  //> es  : java.util.ArrayList[Object] = [2, 1, 1.24, 012, null]

  Ordering.natural[java.lang.String]().nullsFirst[java.lang.String]().onResultOf[Object](Functions.constant(null)).
    nullsLast[Object]().sortedCopy(es)            //> res5: java.util.List[Object] = [2, 1, 1.24, 012, null]
}