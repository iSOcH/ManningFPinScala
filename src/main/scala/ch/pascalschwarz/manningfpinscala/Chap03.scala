package ch.pascalschwarz.manningfpinscala

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
}

object ex03_01_Match {
  val l = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x // won't match, because after 2 comes 3
    case Nil => 42 // list is not empty
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // should match, result 3
    case Cons(h, t) => h + List.sum(t) // would match, but after another matching case
    case _ => 101 // would match, but after another matching case
  }
}

object ex03_02_Tail {
  def tail[A](l: List[A]) = l match {
    case Cons(_, t) => t
    case _ => Nil // other possibility: throw ... simplest way: no case for Nil
  }
}

object ex03_03_SetHead {
  def setHead[A](l: List[A], h: A): List[A] = Cons(h, ex03_02_Tail.tail(l))
}

object ex03_04_Drop {
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n==0) l
    else drop(ex03_02_Tail.tail(l), n-1)
  }
}

object ex03_05_DropWhile {
  @tailrec
  def dropWhile[A](l: List[A])(p: A => Boolean): List[A] = l match {
    case Cons(h, t) if p(h) => dropWhile(t)(p)
    case _ => l
  }
}

object ex03_06_Init {
  def initSlow[A](l: List[A]): List[A] = l match {
    case Cons(h, Cons(_, Nil)) => Cons(h, Nil)
    case Cons(h, t) => Cons(h, init(t))
    case Nil => Nil
  }

  // solution uses listbuffer - more efficient, but needs sideeffects
  def init[A](l: List[A]): List[A] = {
    @tailrec
    def loop(list: List[A], acc: List[A]): List[A] = list match {
      case Nil => acc
      case Cons(h, Cons(m, Nil)) => loop(Nil, Cons(h, acc))
      case Cons(h, t) => loop(t, Cons(h,acc))
    }

    @tailrec
    def reverse(toRev: List[A], acc: List[A]): List[A] = toRev match {
      case Nil => acc
      case Cons(h, t) => reverse(t, Cons(h, acc))
    }

    reverse(loop(l, Nil), Nil)
  }
}