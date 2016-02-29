package ca.fourofclubs.playground

object Eithers {
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(a) => Right(f(a))
      case Left(e)  => Left(e)
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => f(a)
      case Left(e)  => Left(e)
    }
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => Right(a)
      case Left(e)  => b
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
      case Right(a) => b map (f(a, _))
      case Left(e)  => Left(e)
    }
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  Right(7).map { _ * 2 }                          //> res0: ca.fourofclubs.playground.Eithers.Either[Nothing,Int] = Right(14)

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case List()  => Right(List())
    case x :: xs => x.map2(sequence(xs))(_ :: _)
  }                                               //> sequence: [E, A](es: List[ca.fourofclubs.playground.Eithers.Either[E,A]])ca.
                                                  //| fourofclubs.playground.Eithers.Either[E,List[A]]
  sequence(List(Right(1), Right(10), Right(20), Left("Error!"), Left("Problem!")))
                                                  //> res1: ca.fourofclubs.playground.Eithers.Either[String,List[Int]] = Left(Err
                                                  //| or!)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case List()  => Right(List())
    case x :: xs => f(x).flatMap { b => traverse(xs)(f).map(bs => b :: bs) }
  }                                               //> traverse: [E, A, B](as: List[A])(f: A => ca.fourofclubs.playground.Eithers.
                                                  //| Either[E,B])ca.fourofclubs.playground.Eithers.Either[E,List[B]]

  def half(x: Int) = if (x % 2 == 0) Right(x / 2) else Left("Cannot halve '" + x + "'.")
                                                  //> half: (x: Int)Product with Serializable with ca.fourofclubs.playground.Eith
                                                  //| ers.Either[String,Int]
  traverse(List(4, 2, 7))(half)                   //> res2: ca.fourofclubs.playground.Eithers.Either[String,List[Int]] = Left(Can
                                                  //| not halve '7'.)
}