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
  def apply[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a,b) match {
    case (Some(x), Some(y)) => Some(f(x,y))
    case _ => None
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