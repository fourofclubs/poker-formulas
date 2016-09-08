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
  case object None extends Option[Nothing];import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(699); val res$0 = 

  Some(1) getOrElse 2;System.out.println("""res0: Int = """ + $show(res$0));$skip(19); val res$1 = 
  None getOrElse 2;System.out.println("""res1: Int = """ + $show(res$1));$skip(65); val res$2 = 
  Some(2) flatMap { a => if (a % 2 == 0) Some(a / 2) else None };System.out.println("""res2: ca.fourofclubs.utils.collections.Options.Option[Int] = """ + $show(res$2));$skip(34); val res$3 = 

  Some(4) filter { _ % 2 == 0 };System.out.println("""res3: ca.fourofclubs.utils.collections.Options.Option[Int] = """ + $show(res$3));$skip(99); 

  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length);System.out.println("""mean: (xs: Seq[Double])ca.fourofclubs.utils.collections.Options.Option[Double]""");$skip(117); 
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap { m => mean(xs.map { x => math.pow(x - m, 2) }) };System.out.println("""variance: (xs: Seq[Double])ca.fourofclubs.utils.collections.Options.Option[Double]""");$skip(21); val res$4 = 

  variance(List());System.out.println("""res4: ca.fourofclubs.utils.collections.Options.Option[Double] = """ + $show(res$4));$skip(115); 

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(x => b.map(y => f(x, y)));System.out.println("""map2: [A, B, C](a: ca.fourofclubs.utils.collections.Options.Option[A], b: ca.fourofclubs.utils.collections.Options.Option[B])(f: (A, B) => C)ca.fourofclubs.utils.collections.Options.Option[C]""");$skip(49); val res$5 = 
  map2(None, Some(3))((a: Int, b: Int) => a + b);System.out.println("""res5: ca.fourofclubs.utils.collections.Options.Option[Int] = """ + $show(res$5));$skip(156); 

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case List()  => Some(List())
    case x :: xs => map2(x, sequence(xs))(_ :: _)
  };System.out.println("""sequence: [A](a: List[ca.fourofclubs.utils.collections.Options.Option[A]])ca.fourofclubs.utils.collections.Options.Option[List[A]]""");$skip(43); val res$6 = 

  sequence(List(Some(1), Some(2), None));System.out.println("""res6: ca.fourofclubs.utils.collections.Options.Option[List[Int]] = """ + $show(res$6));$skip(197); 

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case List()  => Some(List())
    case x :: xs => f(x).flatMap { b => traverse(xs)(f).map(bs => b :: bs) }
  };System.out.println("""traverse: [A, B](a: List[A])(f: A => ca.fourofclubs.utils.collections.Options.Option[B])ca.fourofclubs.utils.collections.Options.Option[List[B]]""");$skip(35); val res$7 = 
  traverse(List(1, 2, 3))(Some(_));System.out.println("""res7: ca.fourofclubs.utils.collections.Options.Option[List[Int]] = """ + $show(res$7))}
}
