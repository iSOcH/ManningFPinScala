package ch.pascalschwarz.manningfpinscala.Chap05Streams

import scala.annotation.tailrec
import scala.collection.immutable.{Stream => _}

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Cons(a, _) => Some(a())
    case _ => None
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
}