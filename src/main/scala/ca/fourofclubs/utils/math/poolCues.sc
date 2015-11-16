package ca.fourofclubs.utils.math

object poolCues {
  val nums = List(1, 3, 4, 5, 6, 9, 10, 11, 12, 13, 14, 15)
                                                  //> nums  : List[Int] = List(1, 3, 4, 5, 6, 9, 10, 11, 12, 13, 14, 15)
  nums.permutations.filter {
    _ match {
      case List(a, b, c, d, e, f, g, h, i, j, k, l) ⇒ {
        a + 7 == d &&
        a + b == 8 &&
        b + c == f &&
        8 + e == i &&
        f + 2 == j &&
        g + h == k &&
        h + i == l
      }
    }
  }.foreach { println(_) }                        //> List(3, 5, 4, 10, 6, 9, 12, 1, 14, 11, 13, 15)|
}