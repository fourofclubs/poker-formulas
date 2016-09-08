package ca.fourofclubs.utils.collections

import scala.annotation.tailrec

object Lists {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(309); 

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec def loop(as: List[A], acc: B): B = as match {
      case List()  => acc
      case x :: xs => loop(xs, f(acc, x))
    }
    loop(as, z)
  };System.out.println("""foldLeft: [A, B](as: List[A], z: B)(f: (B, A) => B)B""");$skip(76); 
  def reverse[A](as: List[A]) = foldLeft(as, List[A]())((as, b) => b :: as);System.out.println("""reverse: [A](as: List[A])List[A]""");$skip(25); val res$0 = 
  reverse(List(1, 2, 3));System.out.println("""res0: List[Int] = """ + $show(res$0));$skip(110); 

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((as, b) => f(b, as));System.out.println("""foldRight: [A, B](as: List[A], z: B)(f: (A, B) => B)B""");$skip(39); val res$1 = 

  foldRight(List(1, 2, 3), 1)(_ * _);System.out.println("""res1: Int = """ + $show(res$1));$skip(60); val res$2 = 
  foldRight(List(1, 2, 3), List[Int]())((x, xs) => x :: xs);System.out.println("""res2: List[Int] = """ + $show(res$2));$skip(47); val res$3 = 
  foldRight(List(1, 3, 4), 0)((a, b) => 1 + b);System.out.println("""res3: Int = """ + $show(res$3));$skip(70); 

  def append[A](as: List[A])(a: A) = foldRight(as, List(a))(_ :: _);System.out.println("""append: [A](as: List[A])(a: A)List[A]""");$skip(27); val res$4 = 
  append(List(1, 2, 3))(4);System.out.println("""res4: List[Int] = """ + $show(res$4));$skip(83); 

  def flatten[A](ls: List[List[A]]): List[A] = foldRight(ls, List[A]())(_ ::: _);System.out.println("""flatten: [A](ls: List[List[A]])List[A]""");$skip(49); val res$5 = 
  flatten(List(List(1, 2), List(3, 4), List(5)));System.out.println("""res5: List[Int] = """ + $show(res$5));$skip(71); 

  def addOne(is: List[Int]) = foldRight(is, List[Int]())(_ + 1 :: _);System.out.println("""addOne: (is: List[Int])List[Int]""");$skip(24); val res$6 = 
  addOne(List(1, 2, 3));System.out.println("""res6: List[Int] = """ + $show(res$6));$skip(82); 

  def toString[A](as: List[A]) = foldRight(as, List[String]())(_.toString :: _);System.out.println("""toString: [A](as: List[A])List[String]""");$skip(33); val res$7 = 
  toString(List(1.0, 2.0, 3, 0));System.out.println("""res7: List[String] = """ + $show(res$7));$skip(89); 

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, List[B]())(f(_) :: _);System.out.println("""map: [A, B](as: List[A])(f: A => B)List[B]""");$skip(28); val res$8 = 
  map(List(1, 2, 3))(_ + 1);System.out.println("""res8: List[Int] = """ + $show(res$8));$skip(100); 

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, List[B]())(f(_) ::: _);System.out.println("""flatMap: [A, B](as: List[A])(f: A => List[B])List[B]""");$skip(42); val res$9 = 
  flatMap(List(1, 2, 3))(i => List(i, i));System.out.println("""res9: List[Int] = """ + $show(res$9));$skip(120); 

  def filter[A](as: List[A])(p: A => Boolean): List[A] = flatMap(as) { x =>
    if (p(x)) List(x)
    else List()
  };System.out.println("""filter: [A](as: List[A])(p: A => Boolean)List[A]""");$skip(38); val res$10 = 

  filter(List(1, 2, 3))(_ % 2 == 1);System.out.println("""res10: List[Int] = """ + $show(res$10));$skip(199); 

  def add(xs: List[Int], ys: List[Int]): List[Int] = xs match {
    case List() => ys
    case a :: as => ys match {
      case List()  => xs
      case b :: bs => (a + b) :: add(as, bs)
    }
  };System.out.println("""add: (xs: List[Int], ys: List[Int])List[Int]""");$skip(35); val res$11 = 

  add(List(1, 2), List(4, 5, 6));System.out.println("""res11: List[Int] = """ + $show(res$11));$skip(223); 

  def zipWith[A](xs: List[A], ys: List[A])(f: (A, A) => A): List[A] = xs match {
    case List() => ys
    case a :: as => ys match {
      case List()  => xs
      case b :: bs => f(a, b) :: zipWith(as, bs)(f)
    }
  };System.out.println("""zipWith: [A](xs: List[A], ys: List[A])(f: (A, A) => A)List[A]""");$skip(46); val res$12 = 

  zipWith(List(1, 2, 3), List(4, 5))(_ * _);System.out.println("""res12: List[Int] = """ + $show(res$12));$skip(415); 

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startsWith(xs: List[A], ys: List[A]): Boolean = ys match {
      case List() => true
      case y :: ys1 => xs match {
        case List()   => false
        case x :: xs1 => x == y && startsWith(xs1, ys1)
      }
    }
    sup match {
      case List()  => false
      case s :: ss => startsWith(sup, sub) || startsWith(ss, sub)
    }
  };System.out.println("""hasSubsequence: [A](sup: List[A], sub: List[A])Boolean""");$skip(47); val res$13 = 
  hasSubsequence(List(1, 2, 3, 4), List(2, 4));System.out.println("""res13: Boolean = """ + $show(res$13));$skip(50); val res$14 = 
  hasSubsequence(List(1, 2, 3, 4), List(2, 3, 4));System.out.println("""res14: Boolean = """ + $show(res$14))}
}
