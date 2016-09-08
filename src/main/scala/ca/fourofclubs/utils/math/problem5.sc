package ca.fourofclubs.utils.math

import java.util.Collections.EmptySet

object problem5 {
  def f(ns: List[Int], target: Int): List[String] = {
    if (ns.size == 1) { if (ns.head == target) List(ns.head.toString) else List() }
    else {
      val last = ns.last
      val posList = f(ns.init, target - last).map(_ + "+" + last)
      val negList = f(ns.init, target + last).map(_ + "-" + last)
      val concatList = f(ns.take(ns.size - 2) :+ (ns.takeRight(2) :\ "")(_ + _).toInt, target)
      posList ++ negList ++ concatList
    }
  }                                               //> f: (ns: List[Int], target: Int)List[String]

  f(1 to 9 toList, 100).foreach(println(_))       //> 1+23-4+56+7+8+9
                                                  //| 12+3-4+5+67+8+9
                                                  //| 1+2+34-5+67-8+9
                                                  //| 1+2+3-4+5+6+78+9
                                                  //| 123-4-5-6-7+8-9
                                                  //| 123+45-67+8-9
                                                  //| 1+23-4+5+6+78-9
                                                  //| 12-3-4+5-6+7+89
                                                  //| 12+3+4+5-6-7+89
                                                  //| 123-45-67+89
                                                  //| 123+4-5+67-89

  def g(ns: List[Int], target: Int): List[String] = ns match {
    case List()  ⇒ List()
    case List(y) ⇒ if (target == y) List(y.toString) else List()
    case ys :+ x :+ y ⇒ g(ys :+ x, target - y).map(_ + "+" + y) :::
      g(ys :+ x, target + y).map(_ + "-" + y) :::
      g(ys :+ (x.toString + y.toString).toInt, target)
  }                                               //> g: (ns: List[Int], target: Int)List[String]
  g(1 to 9 toList, 100).foreach(println(_))       //> 1+23-4+56+7+8+9
                                                  //| 12+3-4+5+67+8+9
                                                  //| 1+2+34-5+67-8+9
                                                  //| 1+2+3-4+5+6+78+9
                                                  //| 123-4-5-6-7+8-9
                                                  //| 123+45-67+8-9
                                                  //| 1+23-4+5+6+78-9
                                                  //| 12-3-4+5-6+7+89
                                                  //| 12+3+4+5-6-7+89
                                                  //| 123-45-67+89
                                                  //| 123+4-5+67-89
}