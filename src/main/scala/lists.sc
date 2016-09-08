object lists {
  def init[T](xs: List[T]): List[T] = xs match {
    case List()  ⇒ throw new Error("init of empty list")
    case List(x) ⇒ List()
    case y :: ys ⇒ y :: init(ys)
  }                                               //> init: [T](xs: List[T])List[T]

  init(List(1, 2, 3, 4))                          //> res0: List[Int] = List(1, 2, 3)

  def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
    case List()  ⇒ throw new NoSuchElementException
    case y :: ys ⇒ if (n == 0) ys else y :: removeAt(n - 1, ys)
  }                                               //> removeAt: [T](n: Int, xs: List[T])List[T]
  removeAt(2, List('a', 'b', 'c', 'd'))           //> res1: List[Char] = List(a, b, d)

  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() ⇒ List()
    case y :: ys ⇒ y match {
      case x: Iterable[Any] ⇒ flatten(x.toList) ::: flatten(ys)
      case _                ⇒ y :: flatten(ys)
    }
  }                                               //> flatten: (xs: List[Any])List[Any]

  flatten(List(List(1, 1), 2, List(3, List(5, 8))))
                                                  //> res2: List[Any] = List(1, 1, 2, 3, 5, 8)

  def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil     ⇒ xs
    case y :: ys ⇒ (y * y) :: squareList(ys)
  }                                               //> squareList: (xs: List[Int])List[Int]
  def squareList2(xs: List[Int]): List[Int] = xs map (x ⇒ x * x)
                                                  //> squareList2: (xs: List[Int])List[Int]

  squareList(List(1, 2, 3, 4, 5))                 //> res3: List[Int] = List(1, 4, 9, 16, 25)
  squareList2(List(1, 2, 3, 4, 5))                //> res4: List[Int] = List(1, 4, 9, 16, 25)

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil ⇒ Nil
    case x :: xs1 ⇒ {
      val (head, tail) = xs span (y ⇒ x == y)
      head :: pack(tail)
    }
  }                                               //> pack: [T](xs: List[T])List[List[T]]

  val test = List("a", "a", "a", "b", "c", "c", "a")
                                                  //> test  : List[String] = List(a, a, a, b, c, c, a)
  pack(test)                                      //> res5: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a)
                                                  //| )
  def encode[T](xs: List[T]): List[(T, Int)] = pack(xs) map (x => (x.head, x.size))
                                                  //> encode: [T](xs: List[T])List[(T, Int)]
  
  encode(test)                                    //> res6: List[(String, Int)] = List((a,3), (b,1), (c,2), (a,1))
}