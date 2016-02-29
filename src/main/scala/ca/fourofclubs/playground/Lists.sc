package ca.fourofclubs.utils.collections

import scala.annotation.tailrec

object Lists {

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec def loop(as: List[A], acc: B): B = as match {
      case List()  => acc
      case x :: xs => loop(xs, f(acc, x))
    }
    loop(as, z)
  }                                               //> foldLeft: [A, B](as: List[A], z: B)(f: (B, A) => B)B
  def reverse[A](as: List[A]) = foldLeft(as, List[A]())((as, b) => b :: as)
                                                  //> reverse: [A](as: List[A])List[A]
  reverse(List(1, 2, 3))                          //> res0: List[Int] = List(3, 2, 1)

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((as, b) => f(b, as))
                                                  //> foldRight: [A, B](as: List[A], z: B)(f: (A, B) => B)B

  foldRight(List(1, 2, 3), 1)(_ * _)              //> res1: Int = 6
  foldRight(List(1, 2, 3), List[Int]())((x, xs) => x :: xs)
                                                  //> res2: List[Int] = List(1, 2, 3)
  foldRight(List(1, 3, 4), 0)((a, b) => 1 + b)    //> res3: Int = 3

  def append[A](as: List[A])(a: A) = foldRight(as, List(a))(_ :: _)
                                                  //> append: [A](as: List[A])(a: A)List[A]
  append(List(1, 2, 3))(4)                        //> res4: List[Int] = List(1, 2, 3, 4)

  def flatten[A](ls: List[List[A]]): List[A] = foldRight(ls, List[A]())(_ ::: _)
                                                  //> flatten: [A](ls: List[List[A]])List[A]
  flatten(List(List(1, 2), List(3, 4), List(5)))  //> res5: List[Int] = List(1, 2, 3, 4, 5)

  def addOne(is: List[Int]) = foldRight(is, List[Int]())(_ + 1 :: _)
                                                  //> addOne: (is: List[Int])List[Int]
  addOne(List(1, 2, 3))                           //> res6: List[Int] = List(2, 3, 4)

  def toString[A](as: List[A]) = foldRight(as, List[String]())(_.toString :: _)
                                                  //> toString: [A](as: List[A])List[String]
  toString(List(1.0, 2.0, 3, 0))                  //> res7: List[String] = List(1.0, 2.0, 3.0, 0.0)

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, List[B]())(f(_) :: _)
                                                  //> map: [A, B](as: List[A])(f: A => B)List[B]
  map(List(1, 2, 3))(_ + 1)                       //> res8: List[Int] = List(2, 3, 4)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, List[B]())(f(_) ::: _)
                                                  //> flatMap: [A, B](as: List[A])(f: A => List[B])List[B]
  flatMap(List(1, 2, 3))(i => List(i, i))         //> res9: List[Int] = List(1, 1, 2, 2, 3, 3)

  def filter[A](as: List[A])(p: A => Boolean): List[A] = flatMap(as) { x =>
    if (p(x)) List(x)
    else List()
  }                                               //> filter: [A](as: List[A])(p: A => Boolean)List[A]

  filter(List(1, 2, 3))(_ % 2 == 1)               //> res10: List[Int] = List(1, 3)

  def add(xs: List[Int], ys: List[Int]): List[Int] = xs match {
    case List() => ys
    case a :: as => ys match {
      case List()  => xs
      case b :: bs => (a + b) :: add(as, bs)
    }
  }                                               //> add: (xs: List[Int], ys: List[Int])List[Int]

  add(List(1, 2), List(4, 5, 6))                  //> res11: List[Int] = List(5, 7, 6)

  def zipWith[A](xs: List[A], ys: List[A])(f: (A, A) => A): List[A] = xs match {
    case List() => ys
    case a :: as => ys match {
      case List()  => xs
      case b :: bs => f(a, b) :: zipWith(as, bs)(f)
    }
  }                                               //> zipWith: [A](xs: List[A], ys: List[A])(f: (A, A) => A)List[A]

  zipWith(List(1, 2, 3), List(4, 5))(_ * _)       //> res12: List[Int] = List(4, 10, 3)

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
  }                                               //> hasSubsequence: [A](sup: List[A], sub: List[A])Boolean
  hasSubsequence(List(1, 2, 3, 4), List(2, 4))    //> res13: Boolean = false
  hasSubsequence(List(1, 2, 3, 4), List(2, 3, 4)) //> res14: Boolean = true
}