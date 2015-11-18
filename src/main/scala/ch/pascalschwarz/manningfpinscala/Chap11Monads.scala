package ch.pascalschwarz.manningfpinscala

import ch.pascalschwarz.manningfpinscala.Chap08Testing.Gen
import ch.pascalschwarz.manningfpinscala.Chap07FunctionalParallelism.NonBlocking.NonBlocking.Par
import ch.pascalschwarz.manningfpinscala.Chap09Parsers.Parsers

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A,B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))
  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A,B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object FunctorInstances {
  def list = new Functor[List] {
    def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa map f
  }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  override def map[A, B](ma: F[A])(f: (A) => B): F[B] = flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a,b)))
}

object MonadInstances {
  val gen = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](ma: Gen[A])(f: (A) => Gen[B]): Gen[B] = ma.flatMap(f)
  }

  // ex11_01
  val par = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }
  def parser[P[+_]](p: Parsers[P]) = new Monad[P] {
    override def unit[A](a: => A): P[A] = p.succeed(a)
    override def flatMap[A, B](ma: P[A])(f: (A) => P[B]): P[B] = p.flatMap(ma)(f)
  }
  val option = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma flatMap f
  }
  val stream = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)
    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ma flatMap f
  }
  val list = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = ma flatMap f
  }
}