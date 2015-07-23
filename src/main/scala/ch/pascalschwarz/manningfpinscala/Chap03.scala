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

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)
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

object ex03_07_FoldRShortcircuitProduct {
  // does not work, because foldRight recurses before f is applied

  // non-FP:
  def productUgly(l: List[Int]): Int = {
    object Annihilation extends Exception

    def multOrFail(a: Int, b: Int): Int = {
      if (a == 0 || b == 0) throw Annihilation
      else a * b
    }

    try {
      List.foldRight(l, 1)(multOrFail)
    } catch {
      case Annihilation => 0
    }
  }
}

object ex03_08_NilConsFoldR {
  val tryThis: List[Int] = List.foldRight(Cons(1,Cons(2,Cons(3,Nil))), Nil:List[Int])(Cons(_,_))
}

object ex03_09_LengthFold {
  def length[A](as: List[A]): Int = {
    List.foldRight(as, 0){case (_, n:Int) => n + 1}
  }
}

object ex03_10_FoldLeft {
  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }
}

object ex03_11_FoldLeftUsage {
  def sum(as: List[Int]) = ex03_10_FoldLeft.foldLeft(as, 0)(_ + _)
  def product(as: List[Int]) = ex03_10_FoldLeft.foldLeft(as, 1)(_ * _)
  def length[A](as: List[A]) = ex03_10_FoldLeft.foldLeft(as, 0)((n, _) => n + 1)
}

object ex03_12_Reverse {
  def reverse[A](as: List[A]) = ex03_10_FoldLeft.foldLeft(as, Nil:List[A]){(acc, l) => Cons(l, acc)}
}

object ex03_13_FoldInTermsOfEachother {
  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B) = {
    ex03_10_FoldLeft.foldLeft(ex03_12_Reverse.reverse(as), z)((b,a) => f(a,b))
  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B) = {
    ??? // TODO
  }
}