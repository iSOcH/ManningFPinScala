package ch.pascalschwarz.manningfpinscala.Chap05Streams

import scala.annotation.tailrec
import scala.collection.immutable.{Stream => _}

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Cons(a, _) => Some(a())
    case _ => None
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existsFold(p: A => Boolean): Boolean = {
    foldRight(false)(p(_) || _)
  }

  // ex05_01
  def toListRec: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRec
    case Empty => List()
  }

  def toList: List[A] = {
    @tailrec
    def loop(toGo: Stream[A], acc: List[A]): List[A] = toGo match {
      case Empty => acc
      case Cons(h, t) => loop(t(), h() :: acc)
    }

    loop(this, List()).reverse
  }

  // ex05_02
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n-1))
    case Cons(h, _) if n==1 => Stream.cons(h(), Stream.empty)
    case _ => Empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  // ex05_03
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t() takeWhile p)
    case _ => Stream.empty
  }

  // ex05_0? : a stack-safe exists
  @tailrec // IntelliJ thinks its not
  final def exists(p: A => Boolean): Boolean = this match {
    case Empty => false
    case Cons(h, t) => if (p(h())) true else t() exists p
  }

  // ex05_04
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)(p(_) && _)
  }

  // ex05_05
  def takeWhileFold(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A]){ (h, t) =>
      if (p(h)) Stream.cons(h, t)
      else Stream.empty
    }
  }

  // ex05_06
  def headOptionFold: Option[A] = {
    foldRight(None: Option[A])((h, _) => Some(h))
  }

  // ex05_07
  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B]){(h,t) => Stream.cons(f(h), t)} // t is already mapped over
  }
  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A]){(h,t) =>
      if (p(h)) Stream.cons(h, t)
      else t // t is already filtered!
    }
  }
  def append[B >: A](other: => Stream[B]): Stream[B] = {
    foldRight(other){(h,t) => Stream.cons(h, t)}
  }
  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B]){ (h,t) => f(h).append(t) }
  }

  // ex05_13
  def mapUF[B](f: A => B): Stream[B] = {
    Stream.unfold(this){
      case Cons(h,t) => Some{ (f(h()), t()) }
      case Empty => None
    }
  }
  def takeUF(n: Int): Stream[A] = Stream.unfold((this,n)) {
    case (Cons(h, _), 1) => Some{ (h(), (Stream.empty, 0)) }
    case (Cons(h, t), n) if n > 1 => Some{ (h(), (t(), (n-1))) }
    case _ => None
  }
  def takeWhileUF(p: A => Boolean) = Stream.unfold(this) {
    case Cons(h, t) if p(h()) => Some{ (h(), t()) }
    case _ => None
  }
}
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]
case object Empty extends Stream[Nothing]

object Stream {
  def empty[A]: Stream[A] = Empty
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) Empty
    else cons(as.head, apply(as.tail: _*))
  }

  // ex05_08
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // ex05_09
  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  // ex05_10
  def fibs: Stream[Int] = {
    def make(n1: Int, n2: Int): Stream[Int] = cons(n1+n2, make(n2, n1+n2))
    make(0,1)
  }

  // ex05_11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map{
      case (a,s) => cons(a, unfold(s)(f))
    }.getOrElse(empty)
  }

  // ex05_12
  def constantUnfold[A](a: A): Stream[A] = {
    unfold(())(_ => Some(a, ())) // State is Unit - there is no state
  }
  def onesUnfold: Stream[Int] = constantUnfold(1)
  def fromUnfold(n: Int): Stream[Int] = unfold(n-1){
    i => Some(i+1,i+1)
  }
  def fibsUnfold: Stream[Int] = unfold((0,1)){
    case (n1,n2) => Some{ (n1+n2, (n2, n1+n2)) }
  }
}