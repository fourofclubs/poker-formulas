package ca.fourofclubs.utils.collections

object Options {
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case Some(a) => Some(f(a))
      case None    => None
    }
    def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
    def getOrElse[B >: A](default: => B): B = this match {
      case Some(a) => a
      case None    => default
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] = this map (a => Some(a)) getOrElse ob
    def filter(f: A => Boolean): Option[A] = if (this map f getOrElse false) this else None
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  Some(1) getOrElse 2                             //> res0: Int = 1
  None getOrElse 2                                //> res1: Int = 2
  Some(2) flatMap { a => if (a % 2 == 0) Some(a / 2) else None }
                                                  //> res2: ca.fourofclubs.utils.collections.Options.Option[Int] = Some(1)

  Some(4) filter { _ % 2 == 0 }                   //> res3: ca.fourofclubs.utils.collections.Options.Option[Int] = Some(4)

  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)
                                                  //> mean: (xs: Seq[Double])ca.fourofclubs.utils.collections.Options.Option[Doubl
                                                  //| e]
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap { m => mean(xs.map { x => math.pow(x - m, 2) }) }
                                                  //> variance: (xs: Seq[Double])ca.fourofclubs.utils.collections.Options.Option[
                                                  //| Double]

  variance(List())                                //> res4: ca.fourofclubs.utils.collections.Options.Option[Double] = None

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(x => b.map(y => f(x, y)))
                                                  //> map2: [A, B, C](a: ca.fourofclubs.utils.collections.Options.Option[A], b: c
                                                  //| a.fourofclubs.utils.collections.Options.Option[B])(f: (A, B) => C)ca.fourof
                                                  //| clubs.utils.collections.Options.Option[C]
  map2(None, Some(3))((a: Int, b: Int) => a + b)  //> res5: ca.fourofclubs.utils.collections.Options.Option[Int] = None

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case List()  => Some(List())
    case x :: xs => map2(x, sequence(xs))(_ :: _)
  }                                               //> sequence: [A](a: List[ca.fourofclubs.utils.collections.Options.Option[A]])c
                                                  //| a.fourofclubs.utils.collections.Options.Option[List[A]]

  sequence(List(Some(1), Some(2), None))          //> res6: ca.fourofclubs.utils.collections.Options.Option[List[Int]] = None

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case List()  => Some(List())
    case x :: xs => f(x).flatMap { b => traverse(xs)(f).map(bs => b :: bs) }
  }                                               //> traverse: [A, B](a: List[A])(f: A => ca.fourofclubs.utils.collections.Optio
                                                  //| ns.Option[B])ca.fourofclubs.utils.collections.Options.Option[List[B]]
  traverse(List(1, 2, 3))(Some(_))                //> res7: ca.fourofclubs.utils.collections.Options.Option[List[Int]] = Some(Lis
                                                  //| t(1, 2, 3))
}