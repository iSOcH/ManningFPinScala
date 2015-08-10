package ch.pascalschwarz.manningfpinscala.Chap07FunctionalParallelism

import java.util.concurrent.{Callable, TimeUnit, ExecutorService, Future}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] { override def call = a(es).get })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isCancelled: Boolean = false
    override def get(l: Long, timeUnit: TimeUnit): A = get
    override def cancel(b: Boolean): Boolean = false
    override def isDone: Boolean = true
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }
  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))
  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = ps.map(asyncF(f))
    sequence(fbs)
  }
  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  // ex07_04
  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  // ex07_05
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight{unit(List[A]())}{(h,t) => map2(h, t)(_ :: _)}
  }

  // ex07_06
  def parFilter[A](as: List[A])(p: A => Boolean): Par[List[A]] = {
    val pars = as.map(asyncF(a => if (p(a)) List[A](a) else List[A]()))
    map(sequence(pars))(_.flatten)
  }
}