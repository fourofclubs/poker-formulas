package ca.fourofclubs.utils.collections

import com.google.common.collect.Ordering
import com.google.common.collect.Lists
import com.google.common.base.Functions

object scratch {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(237); 
  val list = "testing" :: "something" :: "Test" :: Nil;System.out.println("""list  : ca.fourofclubs.utils.collections.LinkedList[Any] = """ + $show(list ));$skip(24); val res$0 = 
  ("Test" :: list).size;System.out.println("""res0: Int = """ + $show(res$0));$skip(22); val res$1 = 
  ("Test" :: list)(2);System.out.println("""res1: Any = """ + $show(res$1));$skip(13); val res$2 = 

  4 :: Nil;System.out.println("""res2: ca.fourofclubs.utils.collections.LinkedList[Any] = """ + $show(res$2));$skip(28); val res$3 = 
  
  

  Iterable(3, 2, 4);System.out.println("""res3: Iterable[Int] = """ + $show(res$3));$skip(32); val res$4 = 
  LinkedList(Iterable(4, 2, 3));System.out.println("""res4: ca.fourofclubs.utils.collections.LinkedList[Any] = """ + $show(res$4));$skip(125); 

  val es = Lists.newArrayList[Object](Integer.valueOf(2), Integer.valueOf(1), java.lang.Double.valueOf(1.24), "012", null);System.out.println("""es  : java.util.ArrayList[Object] = """ + $show(es ));$skip(157); val res$5 = 

  Ordering.natural[java.lang.String]().nullsFirst[java.lang.String]().onResultOf[Object](Functions.constant(null)).
    nullsLast[Object]().sortedCopy(es);System.out.println("""res5: java.util.List[Object] = """ + $show(res$5))}
}
