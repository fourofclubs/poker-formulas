package ca.fourofclubs.playground

object mons {
  val m = Monoids.intAddition                     //> m  : ca.fourofclubs.playground.Monoid[Int] = ca.fourofclubs.playground.Monoid
                                                  //| s$$anon$1@58ba4bb9
  Monoids.foldMapV(IndexedSeq(1, 2, 3, 4, 5, 6), m)(_ + 1)
                                                  //> res0: Int = 27
  Monoids.ordered(IndexedSeq(1, 2, 3, 4, 5, 6))   //> res1: Boolean = true
  Monoids.ordered(IndexedSeq(4, 4, 3, 8, 9, 10))  //> res2: Boolean = false
}