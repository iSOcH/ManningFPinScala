package ch.pascalschwarz.manningfpinscala

import scala.annotation.tailrec

sealed trait BOption[+A] {
  def map[B](f: A => B): BOption[B] = this match {
    case BSome(x) => BSome(f(x))
    case BNone => BNone
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case BSome(x) => x
    case BNone => default
  }

  def flatMap[B](f: A => BOption[B]): BOption[B] = map(f) getOrElse BNone

  def orElse[B >: A](ob: => BOption[B]): BOption[B] = this match {
    // i really dont like the solution from the book, pattern-matching is more readable here
    case BNone => ob
    case _ => this
  }

  def filter(f: A => Boolean): BOption[A] = if (map(f) getOrElse false) this else BNone
}
case object BNone extends BOption[Nothing]
case class BSome[+A](get: A) extends BOption[A]

object ex04_02_Variance {
  def mean(xs: Seq[Double]): BOption[Double] = {
    if (xs.isEmpty) BNone
    else BSome(xs.sum / xs.length)
  }

  def apply(xs: Seq[Double]): BOption[Double] = {
    val m = mean(xs)
    m flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }
}

object ex04_03_Map2 {
  def apply[A,B,C](a: BOption[A], b: BOption[B])(f: (A, B) => C): BOption[C] = (a,b) match {
    case (BSome(x), BSome(y)) => BSome(f(x,y))
    case _ => BNone
  }
}

object ex04_04_Sequence {
  def apply[A](a: List[BOption[A]]): BOption[List[A]] = {

    @tailrec
    def loop(lO: List[BOption[A]], acc: List[A]): BOption[List[A]] = lO match {
      case List() => BSome(acc)
      case BSome(a) :: t => loop(t, a :: acc)
      case _ => BNone
    }

    loop(a, List[A]()).map(_.reverse)
  }
}

object ex04_05_Traverse {
  def apply[A, B](a: List[A])(f: A => BOption[B]): BOption[List[B]] = a match {
    case h :: t => ex04_03_Map2(f(h), apply(t)(f))(_ :: _)
    case Nil => BSome(Nil)
  }

  def usingFoldR[A, B](a: List[A])(f: A => BOption[B]): BOption[List[B]] = {
    //a.foldRight(Some(List[B]()): Option[List[B]]){(x, acc) => ex04_03_Map2(f(x), acc)(_ :: _)}
    a.foldRight[BOption[List[B]]] (BSome(List[B]())) {(x, acc) => ex04_03_Map2(f(x), acc)(_ :: _)}
  }

  def sequence[A](a: List[BOption[A]]): BOption[List[A]] = apply(a)(x => x)
}