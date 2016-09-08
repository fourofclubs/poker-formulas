package ca.fourofclubs.utils.math

object poolCues {
  val nums = List(1, 2, 3, 6, 7, 8, 9, 10, 11, 13, 14, 15)
                                                  //> nums  : List[Int] = List(1, 2, 3, 6, 7, 8, 9, 10, 11, 13, 14, 15)
  nums.permutations.filter {
    _ match {
      case List(a, b, c, d, e, f, g, h, i, j, k, l) ⇒ {
      	4 + a == c &&
        a + b == d &&
        d + e == 12 &&
        e + 5 == h &&
        5 + f == j &&
        g + 12 == k &&
        h + i == l
      }
    }
  }.foreach { println(_) }                        //> List(7, 2, 11, 9, 3, 10, 1, 8, 6, 15, 13, 14)/
}