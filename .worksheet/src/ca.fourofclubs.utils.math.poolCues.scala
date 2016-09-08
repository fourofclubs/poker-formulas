package ca.fourofclubs.utils.math

object poolCues {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(112); 
  val nums = List(1, 2, 3, 6, 7, 8, 9, 10, 11, 13, 14, 15);System.out.println("""nums  : List[Int] = """ + $show(nums ));$skip(292); 
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
  }.foreach { println(_) }}
}
