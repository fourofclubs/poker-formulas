package ca.foc.play.monads

import java.text.{ ParseException, SimpleDateFormat }
import java.util.Date
import ca.foc.play.monoids.Monoid
import scala.{ Left, Right, Stream, Vector }
import ca.foc.play.State
import ca.foc.play.par.Par
import ca.foc.play.parsing.{ Parser, ParsersImpl }
import ca.foc.play.testing.Gen
import ca.foc.play.monoids.Foldable

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))
  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa)  => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  def listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B) = as map f
  }
  def optionFunctor = new Functor[Option] {
    def map[A, B](oa: Option[A])(f: A => B) = oa map f
  }
  def parFunctor = new Functor[Par] {
    def map[A, B](pa: Par[A])(f: A => B) = Par.map(pa)(f)
  }
}

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_(_))
  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(map2(fa, fb)((a, b) => (c: C) => f(a, b, c)))(fc)
  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(map3(fa, fb, fc)((a, b, c) => (d: D) => f(a, b, c, d)))(fd)
  def _apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f: A => B, a: A) => f(a))
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))
  def sequence[A](lfa: List[F[A]]): F[List[A]] = traverse(lfa)(fa => fa)
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map.empty[K, V])) { case ((k, fv), acc) => map2(acc, fv)((m, v) => m + (k -> v)) }
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))
  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    map(sequence(ms.map(a => map(f(a))((_, a)))))(as => as.filter(_._1).map(_._2))
  def product[G[_]](G: Applicative[G]): Applicative[({ type f[x] = (F[x], G[x]) })#f] = {
    val self = this
    new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
      def unit[A](a: ⇒ A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      override def apply[A, B](fab: (F[A ⇒ B], G[A ⇒ B]))(fa: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
    }
  }
  def compose[G[_]](G: Applicative[G]): Applicative[({ type f[x] = F[G[x]] })#f] = {
    val self = this
    new Applicative[({ type f[x] = F[G[x]] })#f] {
      def unit[A](a: ⇒ A): F[G[A]] = self.unit(G.unit(a))
      override def apply[A, B](fab: F[G[A ⇒ B]])(fa: F[G[A]]): F[G[B]] =
        self.map2(fab, fa)(G.apply(_)(_))
    }
  }
}

object Applicative {
  def listApplicative = new Applicative[List] {
    def unit[A](a: ⇒ A): List[A] = List(a)
    override def apply[A, B](fs: List[A ⇒ B])(as: List[A]): List[B] = for (f <- fs; a <- as) yield f(a)
  }
  def optionApplicative = new Applicative[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    override def apply[A, B](of: Option[A => B])(oa: Option[A]): Option[B] = for (f <- of; a <- oa) yield f(a)
  }
  def parApplicative = new Applicative[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    override def apply[A, B](pf: Par[A => B])(pa: Par[A]): Par[B] = Par.map2(pf, pa)((f, a) => f(a))
  }
  def validationApplicative[E] = new Applicative[({ type f[x] = Validation[E, x] })#f] {
    def unit[A](a: => A): Validation[E, A] = Success(a)
    override def apply[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] = (fab, fa) match {
      case (Failure(h, t), Failure(h2, t2)) => Failure(h, h2 +: t ++: t2)
      case (e @ Failure(_, _), Success(_))  => e
      case (Success(_), e @ Failure(_, _))  => e
      case (Success(f), Success(a))         => Success(f(a))
    }
  }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(ga => ga)
  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    type Id[A] = A
    val idMonad = new Monad[Id] {
      def unit[A](a: => A) = a
      override def flatMap[A, B](a: A)(f: A => B): B = f(a)
    }
    traverse[Id, A, B](fa)(f)(idMonad)
  }
}

object Traverse {
  type Const[M, B] = M
  implicit def monoidApplicative[M](M: Monoid[M]) = new Applicative[({ type f[x] = Const[M, x] })#f] {
    def unit[A](a: => A): M = M.zero
    override def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1, m2)
  }
  def listTraverse: Traverse[List] = new Traverse[List] {
    override def traverse[M[_], A, B](as: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
      as.foldRight(M.unit(List[B]()))((a, fbs) => M.map2(f(a), fbs)(_ :: _))
    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) ⇒ B): B = as match {
      case a :: as => f(foldLeft(as)(z)(f), a)
      case List()  => z
    }
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) ⇒ B): B = ???
  }
  def optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def traverse[M[_], A, B](oa: Option[A])(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] = oa match {
      case Some(a) => M.map(f(a))(Some(_))
      case None    => M.unit(None)
    }
    def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) ⇒ B): B = as match {
      case Some(a) => f(z, a)
      case None    => z
    }
    def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) ⇒ B): B = as match {
      case Some(a) => f(a, z)
      case None    => z
    }
  }
  def treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[M[_], A, B](ta: Tree[A])(f: A => M[B])(implicit M: Applicative[M]): M[Tree[B]] =
      M.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
    def foldLeft[A, B](as: ca.foc.play.monads.Tree[A])(z: B)(f: (B, A) ⇒ B): B = ???
    def foldRight[A, B](as: ca.foc.play.monads.Tree[A])(z: B)(f: (A, B) ⇒ B): B = ???
  }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = flatMap(fab)(f => map(fa)(a => f(a)))
  def _flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => fa, f)(())
  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
  def _compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
}

case class Id[A](value: A) {
  def map[B](f: A => B) = Id(f(value))
  def flatMap[B](f: A => Id[B]) = f(value)
}

object Monad {
  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A) = Par.unit(a)
    def flatMap[A, B](p: Par[A])(f: A => Par[B]) = Par.flatMap(p)(f)
  }
  val parserMonad: Monad[Parser] = new Monad[Parser] {
    def unit[A](a: => A) = ParsersImpl.succeed(a)
    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]) = ParsersImpl.flatMap(p)(f)
  }
  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    def flatMap[A, B](o: Option[A])(f: A => Option[B]) = o flatMap f
  }
  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A) = Stream(a)
    def flatMap[A, B](s: Stream[A])(f: A => Stream[B]) = s flatMap f
  }
  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A) = List(a)
    def flatMap[A, B](l: List[A])(f: A => List[B]) = l flatMap f
  }
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A) = Gen.unit(a)
    def flatMap[A, B](g: Gen[A])(f: A => Gen[B]) = g flatMap f
  }
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = Id(a)
    def flatMap[A, B](i: Id[A])(f: A => Id[B]) = i flatMap f
  }
  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A) = State.unit(a)
    def flatMap[A, B](s: State[S, A])(f: A => State[S, B]) = s flatMap f
  }
  def eitherMonad[E] = new Monad[({ type f[x] = Either[E, x] })#f] {
    def unit[A](a: ⇒ A): Either[E, A] = Right(a)
    def flatMap[A, B](fa: Either[E, A])(f: A ⇒ Either[E, B]): Either[E, B] = fa.right flatMap f
  }
}

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

case class WebForm(name: String, birthdate: Date, phoneNumber: String)
object Validation {
  def validName(name: String): Validation[String, String] = if (name != "") Success(name) else Failure("Name cannot be empty")
  def validBirthdate(birthdate: String): Validation[String, Date] =
    try {
      import java.text._
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
    } catch { case e: ParseException => Failure("Birthdate must be in the form 'yyyy-MM-dd'") }
  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}")) Success(phoneNumber)
    else Failure("Phone number must be 10 digits")
  def validWebForm(name: String, birthdate: String, phone: String): Validation[String, WebForm] =
    Applicative.validationApplicative[String].map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phone))(WebForm(_, _, _))
}