package ca.fourofclubs.playground

object Streams {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(86); 
  val str = Stream.apply(1, 2, 3);System.out.println("""str  : ca.fourofclubs.playground.Stream[Int] = """ + $show(str ));$skip(39); val res$0 = 
  Stream("A", "B", "C").take(4).toList;System.out.println("""res0: List[String] = """ + $show(res$0));$skip(48); val res$1 = 
  Stream(1, 2, 3, 4, 5).takeWhile(_ < 5).toList;System.out.println("""res1: List[Int] = """ + $show(res$1));$skip(35); val res$2 = 
  Stream(1, 2, 3, 4, 5).headOption;System.out.println("""res2: Option[Int] = """ + $show(res$2));$skip(37); val res$3 = 
  Stream(1, 2, 3).drop(1).headOption;System.out.println("""res3: Option[Int] = """ + $show(res$3));$skip(36); val res$4 = 
  Stream(1, 2, 3).map(_ * 2).toList;System.out.println("""res4: List[Int] = """ + $show(res$4));$skip(44); val res$5 = 
  Stream(1, 2, 3).filter(_ % 2 == 1).toList;System.out.println("""res5: List[Int] = """ + $show(res$5));$skip(110); val res$6 = 
  //Stream(1, 2, 3).append(4.0).toList
  Stream(1, 2, 3).flatMap(x => Option(x * 2).filter(_ % 3 > 0)).toList;System.out.println("""res6: List[Int] = """ + $show(res$6));$skip(49); 

  def ones: Stream[Int] = Stream.cons(1, ones);System.out.println("""ones: => ca.fourofclubs.playground.Stream[Int]""");$skip(34); val res$7 = 
  ones.map(_ + 1).take(10).toList;System.out.println("""res7: List[Int] = """ + $show(res$7));$skip(74); val res$8 = 
  //Stream.constant("A").take(4).toList
  Stream.from(10).take(10).toList;System.out.println("""res8: List[Int] = """ + $show(res$8));$skip(248); val res$9 = 
  //Stream.fibs.take(10).toList

  //Stream.fibs.zipWith(Stream("B")).take(10).toList
  //Stream.fibs.zipAll(Stream("A", "B")).take(10).toList
  //Stream.fibs.startsWith(Stream(0, 1, 1, 2, 3))
  Stream(1, 2, 3, 4, 5).tails.map { _.toList }.toList;System.out.println("""res9: List[List[Int]] = """ + $show(res$9));$skip(113); val res$10 = 

  //Stream(0, 1, 1, 3, 4).hasSubsequence(Stream(1, 1, 3))

  Stream(0, 1, 2, 3, 4).scanRight(0)(_ + _).toList;System.out.println("""res10: List[Int] = """ + $show(res$10))}
}
