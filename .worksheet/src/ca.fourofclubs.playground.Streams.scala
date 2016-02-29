package ca.fourofclubs.playground

object Streams {
  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(head, tail) => head() :: tail().toList
      case Empty            => List()
    }
    def take(n: Int): Stream[A] = this match {
      case Cons(head, tail) => if (n <= 0) Empty else Cons(head, () => tail().take(n - 1))
      case Empty            => Empty
    }
    def takeWhile(p: A => Boolean): Stream[A] = foldRight[Stream[A]](Empty)((a, b) => if (p(a)) Cons(() => a, () => b) else Empty)
    def drop(n: Int): Stream[A] = this match {
      case Cons(head, tail) => if (n <= 0) this else tail().drop(n - 1)
      case Empty            => Empty
    }
    def forAll(p: A => Boolean): Boolean = this match {
      case Empty            => true
      case Cons(head, tail) => p(head()) && tail().forAll(p)
    }
    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(head, tail) => f(head(), tail().foldRight(z)(f))
      case _                => z
    }
    def headOption = foldRight(Option.empty[A])((a, b) => Option(a))
    def map[B](f: A => B) = foldRight[Stream[B]](Empty)((a, b) => Cons[B](() => f(a), () => b))
    def filter(p: A => Boolean) = foldRight[Stream[A]](Empty)((a, b) => if (p(a)) Cons(() => a, () => b) else b)
    def append[B >: A](a: => B) = foldRight(Cons(() => a, () => Empty))((a, b) => Cons(() => a, () => b))
    def flatMap[B](f: A => Option[B]): Stream[B] =
      foldRight[Stream[B]](Empty)((a, b) => f(a).map(x => Cons(() => x, () => b)).getOrElse(b))
  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty
    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    def constant[A](a: A): Stream[A] = cons(a, constant(a))
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))
    def fibs: Stream[Int] = {
    	def fibsFrom(a:Int, b:Int):Stream[Int] = cons(a, fibsFrom(b, a + b))
    	fibsFrom(0, 1)
    }
  };import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(2259); 

  val str = Stream(1, 2, 3);System.out.println("""str  : ca.fourofclubs.playground.Streams.Stream[Int] = """ + $show(str ));$skip(39); val res$0 = 
  Stream("A", "B", "C").take(4).toList;System.out.println("""res0: List[String] = """ + $show(res$0));$skip(48); val res$1 = 
  Stream(1, 2, 3, 4, 5).takeWhile(_ < 5).toList;System.out.println("""res1: List[Int] = """ + $show(res$1));$skip(38); val res$2 = 
  Stream(1, 2, 3, 4, 5).forAll(_ > 1);System.out.println("""res2: Boolean = """ + $show(res$2));$skip(35); val res$3 = 
  Stream(1, 2, 3, 4, 5).headOption;System.out.println("""res3: Option[Int] = """ + $show(res$3));$skip(37); val res$4 = 
  Stream(1, 2, 3).drop(1).headOption;System.out.println("""res4: Option[Int] = """ + $show(res$4));$skip(36); val res$5 = 
  Stream(1, 2, 3).map(_ * 2).toList;System.out.println("""res5: List[Int] = """ + $show(res$5));$skip(44); val res$6 = 
  Stream(1, 2, 3).filter(_ % 2 == 1).toList;System.out.println("""res6: List[Int] = """ + $show(res$6));$skip(37); val res$7 = 
  Stream(1, 2, 3).append(4.0).toList;System.out.println("""res7: List[AnyVal] = """ + $show(res$7));$skip(71); val res$8 = 
  Stream(1, 2, 3).flatMap(x => Option(x * 2).filter(_ % 3 > 0)).toList;System.out.println("""res8: List[Int] = """ + $show(res$8));$skip(49); 

  def ones: Stream[Int] = Stream.cons(1, ones);System.out.println("""ones: => ca.fourofclubs.playground.Streams.Stream[Int]""");$skip(34); val res$9 = 
  ones.map(_ + 1).take(10).toList;System.out.println("""res9: List[Int] = """ + $show(res$9));$skip(38); val res$10 = 
  Stream.constant("A").take(4).toList;System.out.println("""res10: List[String] = """ + $show(res$10));$skip(34); val res$11 = 
  Stream.from(10).take(10).toList;System.out.println("""res11: List[Int] = """ + $show(res$11));$skip(30); val res$12 = 
  Stream.fibs.take(10).toList;System.out.println("""res12: List[Int] = """ + $show(res$12))}
}
