package ca.fourofclubs.utils.math
import Complex._
import Math._
import Rational._

object scratch {
  def sum1(xs: List[Int]): Int = {
    var s = 0
    xs.foreach { x ⇒ s = s + x }
    s
  }                                               //> sum1: (xs: List[Int])Int
  def sum2(xs: List[Int]): Int = {
    var s = 0
    val itr = xs.iterator
    while (itr.hasNext) { s = s + itr.next }
    s
  }                                               //> sum2: (xs: List[Int])Int
  def sum3(xs: List[Int]): Int = { if (xs.isEmpty) 0 else xs.head + sum3(xs.tail) }
                                                  //> sum3: (xs: List[Int])Int
  sum1(List(1, 3, 5))                             //> res0: Int = 9
  sum2(List(1, 3, 5, 10))                         //> res1: Int = 19
  sum3(List(1, 3, 5))                             //> res2: Int = 9

  def zip(x1: List[Any], x2: List[Any]): List[Any] = {
    if (x1.isEmpty) x2
    else if (x2.isEmpty) x1
    else List(x1.head, x2.head) ++ zip(x1.tail, x2.tail)
  }                                               //> zip: (x1: List[Any], x2: List[Any])List[Any]
  zip(List(0, 1, 2), List('a', 'b', 'c', 'd'))    //> res3: List[Any] = List(0, a, 1, b, 2, c, d)

  implicit def int2BigInt(n: Int) = BigInt(n)     //> int2BigInt: (n: Int)scala.math.BigInt
  def fibList(n: BigInt): List[BigInt] = {
    if (n <= 2) List[BigInt](1, 1).take(n.intValue)
    else {
      val fibs = fibList(n - 1)
      fibs :+ (fibs.takeRight(2) :\ BigInt(0))(_ + _)
    }
  }                                               //> fibList: (n: BigInt)List[BigInt]
  fibList(100)                                    //> res4: List[BigInt] = List(1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 37
                                                  //| 7, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368, 75025, 1213
                                                  //| 93, 196418, 317811, 514229, 832040, 1346269, 2178309, 3524578, 5702887, 9227
                                                  //| 465, 14930352, 24157817, 39088169, 63245986, 102334155, 165580141, 267914296
                                                  //| , 433494437, 701408733, 1134903170, 1836311903, 2971215073, 4807526976, 7778
                                                  //| 742049, 12586269025, 20365011074, 32951280099, 53316291173, 86267571272, 139
                                                  //| 583862445, 225851433717, 365435296162, 591286729879, 956722026041, 154800875
                                                  //| 5920, 2504730781961, 4052739537881, 6557470319842, 10610209857723, 171676801
                                                  //| 77565, 27777890035288, 44945570212853, 72723460248141, 117669030460994, 1903
                                                  //| 92490709135, 308061521170129, 498454011879264, 806515533049393, 130496954492
                                                  //| 8657, 2111485077978050, 3416454622906707, 5527939700884757, 8944394323791464
                                                  //| , 14472334024676221, 23416728348467685, 37889062373143906, 61305790721611591
                                                  //| , 99194853094755497, 160
                                                  //| Output exceeds cutoff limit.

  def largest(xs: List[Int]): Int = {
    val sorted = xs.sortBy { x ⇒ (x.toString.substring(0, 1) + "." + x.toString.substring(1)).toDouble }.reverse
    (sorted :\ "")(_.toString + _).toInt
  }                                               //> largest: (xs: List[Int])Int
	largest(List(3,2,50,9))                   //> res5: Int = 95032
}