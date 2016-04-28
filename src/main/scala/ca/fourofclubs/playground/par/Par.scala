package ca.fourofclubs.playground.par

import java.util.concurrent.{ Callable, ExecutorService, Future, TimeUnit }

case class UnitFuture[A](get: A) extends Future[A] {
  def isDone = true
  def get(timeout: Long, units: TimeUnit) = get
  def isCancelled = false
  def cancel(eventIfRunning: Boolean): Boolean = false
}

object Par {
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }
  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit(List[A]())) {
    (par, pars) => map2(par, pars)(_ :: _)
  }
  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars = as.map(asyncF((a: A) => if (f(a)) Some(a) else None))
    map(sequence(pars))(_.flatten)
  }
  def join[A](a: Par[Par[A]]): Par[A] = es => run(es)(run(es)(a).get())
  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = es => f(run(es)(a).get)(es)
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = flatMap(n)(choices(_))
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = flatMap(cond)(if (_) t else f)
}