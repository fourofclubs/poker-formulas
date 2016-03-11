package ca.fourofclubs.playground

import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit
import java.util.concurrent.Callable
import java.util.concurrent.Executors

object Pars {
  type Par[A] = ExecutorService => Future[A]
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
                                                  //> unit: [A](a: A)ca.fourofclubs.playground.Pars.Par[A]
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })                                            //> fork: [A](a: => ca.fourofclubs.playground.Pars.Par[A])ca.fourofclubs.playgro
                                                  //| und.Pars.Par[A]
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
                                                  //> run: [A](s: java.util.concurrent.ExecutorService)(a: ca.fourofclubs.playgrou
                                                  //| nd.Pars.Par[A])java.util.concurrent.Future[A]
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))//> lazyUnit: [A](a: => A)ca.fourofclubs.playground.Pars.Par[A]

  case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(eventIfRunning: Boolean): Boolean = false
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))
                                                  //> map: [A, B](pa: ca.fourofclubs.playground.Pars.Par[A])(f: A => B)ca.fourofcl
                                                  //| ubs.playground.Pars.Par[B]
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }                                               //> map2: [A, B, C](a: ca.fourofclubs.playground.Pars.Par[A], b: ca.fourofclubs
                                                  //| .playground.Pars.Par[B])(f: (A, B) => C)ca.fourofclubs.playground.Pars.Par[
                                                  //| C]
  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))
                                                  //> asyncF: [A, B](f: A => B)A => ca.fourofclubs.playground.Pars.Par[B]
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit(List[A]())) {
    (par, pars) => map2(par, pars)(_ :: _)
  }                                               //> sequence: [A](ps: List[ca.fourofclubs.playground.Pars.Par[A]])ca.fourofclub
                                                  //| s.playground.Pars.Par[List[A]]
  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }                                               //> parMap: [A, B](ps: List[A])(f: A => B)ca.fourofclubs.playground.Pars.Par[Li
                                                  //| st[B]]
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars = as.map(asyncF((a: A) => if (f(a)) Some(a) else None))
    map(sequence(pars))(_.flatten)
  }                                               //> parFilter: [A](as: List[A])(f: A => Boolean)ca.fourofclubs.playground.Pars.
                                                  //| Par[List[A]]

  val aF = asyncF[Int, Int](_ + 4)                //> aF  : Int => ca.fourofclubs.playground.Pars.Par[Int] = <function1>
  val es = Executors.newCachedThreadPool()        //> es  : java.util.concurrent.ExecutorService = java.util.concurrent.ThreadPoo
                                                  //| lExecutor@a4f0a4f
  aF(5)(es).get                                   //> res0: Int = 9
  unit(5)(es).get                                 //> res1: Int = 5
  sequence(List(aF(5), aF(4), aF(10)))(es).get    //> res2: List[Int] = List(9, 8, 14)
  parFilter(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(_ < 5)(es).get
                                                  //> res3: List[Int] = List(1, 2, 3, 4)
  es.shutdown()

}