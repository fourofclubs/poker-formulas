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
  }

  val str = Stream(1, 2, 3)                       //> str  : ca.fourofclubs.playground.Streams.Stream[Int] = Cons(<function0>,<fu
                                                  //| nction0>)
  Stream("A", "B", "C").take(4).toList            //> res0: List[String] = List(A, B, C)
  Stream(1, 2, 3, 4, 5).takeWhile(_ < 5).toList   //> res1: List[Int] = List(1, 2, 3, 4)
  Stream(1, 2, 3, 4, 5).forAll(_ > 1)             //> res2: Boolean = false
  Stream(1, 2, 3, 4, 5).headOption                //> res3: Option[Int] = Some(1)
  Stream(1, 2, 3).drop(1).headOption              //> res4: Option[Int] = Some(2)
  Stream(1, 2, 3).map(_ * 2).toList               //> res5: List[Int] = List(2, 4, 6)
  Stream(1, 2, 3).filter(_ % 2 == 1).toList       //> res6: List[Int] = List(1, 3)
  Stream(1, 2, 3).append(4.0).toList              //> res7: List[AnyVal] = List(1, 2, 3, 4.0)
  Stream(1, 2, 3).flatMap(x => Option(x * 2).filter(_ % 3 > 0)).toList
                                                  //> res8: List[Int] = List(2, 4)

  def ones: Stream[Int] = Stream.cons(1, ones)    //> ones: => ca.fourofclubs.playground.Streams.Stream[Int]
  ones.map(_ + 1).take(10).toList                 //> res9: List[Int] = List(2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
  Stream.constant("A").take(4).toList             //> res10: List[String] = List(A, A, A, A)
  Stream.from(10).take(10).toList                 //> res11: List[Int] = List(10, 11, 12, 13, 14, 15, 16, 17, 18, 19)
  Stream.fibs.take(10).toList                     //> res12: List[Int] = List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
}