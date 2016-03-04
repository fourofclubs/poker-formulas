package ca.fourofclubs.playground

object Streams {
  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(head, tail) => head() :: tail().toList
      case Empty            => List()
    }
    def take(n: Int): Stream[A] = Stream.unfold((this, n)) {
      case (Empty, _)            => None
      case (_, 0)                => None
      case (Cons(head, tail), n) => Some(head(), (tail(), n - 1))
    }
    def takeWhile(p: A => Boolean): Stream[A] = Stream.unfold(this) {
      case Empty            => None
      case Cons(head, tail) => if (p(head())) Some(head(), tail()) else None
    }
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
    def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)
    def headOption = foldRight(Option.empty[A])((a, b) => Option(a))
    def map[B](f: A => B) = Stream.unfold(this) {
      case Empty            => None
      case Cons(head, tail) => Some((f(head()), tail()))
    }
    def filter(p: A => Boolean) = foldRight[Stream[A]](Empty)((a, b) => if (p(a)) Cons(() => a, () => b) else b)
    def append[B >: A](a: => B) = foldRight(Cons(() => a, () => Empty))((a, b) => Cons(() => a, () => b))
    def flatMap[B](f: A => Option[B]): Stream[B] =
      foldRight[Stream[B]](Empty)((a, b) => f(a).map(x => Cons(() => x, () => b)).getOrElse(b))
    def zipWith[B](bs: Stream[B]): Stream[(A, B)] = Stream.unfold((this, bs)) {
      case (Empty, _)                               => None
      case (_, Empty)                               => None
      case (Cons(aHead, aTail), Cons(bHead, bTail)) => Some((aHead(), bHead()), (aTail(), bTail()))
    }
    def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, bs)) {
      case (Empty, Empty)                           => None
      case (Empty, Cons(head, tail))                => Some((None, Some(head())), (Empty, tail()))
      case (Cons(head, tail), Empty)                => Some((Some(head()), None), (tail(), Empty))
      case (Cons(aHead, aTail), Cons(bHead, bTail)) => Some((Some(aHead()), Some(bHead())), (aTail(), bTail()))
    }
    def startsWith[A](s: Stream[A]): Boolean = zipAll(s).takeWhile(_._2.isDefined).forAll(x => x._1 == x._2)
    def tails: Stream[Stream[A]] = Stream.unfold(this) {
      as =>
        as match {
          case Empty            => None
          case Cons(head, tail) => Some(as, tail())
        }
    }
    def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)
    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = this match {
      case Empty => Stream(z)
      case Cons(head, tail) => {
        val scanTail: Stream[B] = tail().scanRight(z)(f)
        scanTail.headOption.map { f(head(), _) }.map { b => Cons(() => b, () => scanTail) }.getOrElse(Stream())
      }
    }
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
    def constant[A](a: A): Stream[A] = unfold(a)(a => Some(a, a))
    def from(n: Int): Stream[Int] = unfold(n)(a => Some(a, a + 1))
    def fibs: Stream[Int] = unfold((0, 1)) { case (a, b) => Some(a, (b, a + b)) }
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z).map { case (a, s) => cons(a, unfold(s)(f)) }.getOrElse(empty[A])
    def map[A, B](as: Stream[A])(f: A => B) = Stream.unfold(as) {
      case Empty            => None
      case Cons(head, tail) => Some(f(head()), tail())
    }
  };import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(4237); 

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
  Stream.fibs.take(10).toList;System.out.println("""res12: List[Int] = """ + $show(res$12));$skip(53); val res$13 = 

  Stream.fibs.zipWith(Stream("B")).take(10).toList;System.out.println("""res13: List[(Int, String)] = """ + $show(res$13));$skip(55); val res$14 = 
  Stream.fibs.zipAll(Stream("A", "B")).take(10).toList;System.out.println("""res14: List[(Option[Int], Option[String])] = """ + $show(res$14));$skip(48); val res$15 = 
  Stream.fibs.startsWith(Stream(0, 1, 1, 2, 3));System.out.println("""res15: Boolean = """ + $show(res$15));$skip(54); val res$16 = 
  Stream(1, 2, 3, 4, 5).tails.map { _.toList }.toList;System.out.println("""res16: List[List[Int]] = """ + $show(res$16));$skip(58); val res$17 = 

  Stream(0, 1, 1, 3, 4).hasSubsequence(Stream(1, 1, 3));System.out.println("""res17: Boolean = """ + $show(res$17));$skip(53); val res$18 = 

  Stream(0, 1, 2, 3, 4).scanRight(0)(_ + _).toList;System.out.println("""res18: List[Int] = """ + $show(res$18))}
}
