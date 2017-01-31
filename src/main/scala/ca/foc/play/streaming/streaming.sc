package ca.foc.play.streaming
import Process._

object streaming {
  val s1 = Stream.range(1, 10)                    //> s1  : scala.collection.immutable.Stream[Int] = Stream(1, ?)
  val s2 = Stream(1.0, 5.5, 1.5, 6.0, 10.0, -11.0)//> s2  : scala.collection.immutable.Stream[Double] = Stream(1.0, ?)

  val p = liftOne[Int, Int](_ + 7)                //> p  : ca.foc.play.streaming.Process[Int,Int] = Await(<function1>)
  val p2 = filter[Int](_ % 2 == 1)                //> p2  : ca.foc.play.streaming.Process[Int,Int] = Await(<function1>)
  val p3 = sum                                    //> p3  : ca.foc.play.streaming.Process[Double,Double] = Await(<function1>)
  val p4 = lift[Int, Int](_ * 4)                  //> p4  : ca.foc.play.streaming.Process[Int,Int] = Await(<function1>)

  count(s1).toList                                //> res0: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  mean(s2).toList                                 //> res1: List[Double] = List(1.0, 3.25, 2.6666666666666665, 3.5, 4.8, 2.1666666
                                                  //| 666666665)
  mean2(s2).toList                                //> res2: List[Double] = List(1.0, 3.25, 2.6666666666666665, 3.5, 4.8, 2.1666666
                                                  //| 666666665)
	(p3 |> lift(_ + 3))(s2).toList            //> res3: List[Double] = List(4.0, 9.5, 11.0, 17.0, 27.0, 16.0)
	(lift[Int, Int](_ + 4) |> p2)(s1).toList  //> res4: List[Int] = List(5, 7, 9, 11, 13)
	
	(lift((d:Double) => d * 3) |> mean |> zipWithIndex)(s2).toList
                                                  //> res5: List[(Double, Int)] = List((3.0,0), (9.75,1), (8.0,2), (10.5,3), (14.4
                                                  //| ,4), (6.5,5))
	exists[Int](_ % 2 == 0)(s1).toList        //> res6: List[Boolean] = List(false, true)
	
	takeThrough[Int](_ < 2)(s1).toList        //> res7: List[Int] = List(1, 2)
}