package ch.pascalschwarz.manningfpinscala

import scala.annotation.tailrec

sealed trait BookList[+A]
case object Nil extends BookList[Nothing]
case class Cons[+A](head: A, tail: BookList[A]) extends BookList[A]

object BookList {
  def apply[A](as: A*): BookList[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: BookList[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: BookList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def foldRight[A,B](as: BookList[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: BookList[Int]) = foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: BookList[Double]) = foldRight(ns, 1.0)(_ * _)
}

object ex03_01_Match {
  val l = BookList(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x // won't match, because after 2 comes 3
    case Nil => 42 // list is not empty
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // should match, result 3
    case Cons(h, t) => h + BookList.sum(t) // would match, but after another matching case
    case _ => 101 // would match, but after another matching case
  }
}

object ex03_02_Tail {
  def tail[A](l: BookList[A]) = l match {
    case Cons(_, t) => t
    case _ => Nil // other possibility: throw ... simplest way: no case for Nil
  }
}

object ex03_03_SetHead {
  def setHead[A](l: BookList[A], h: A): BookList[A] = Cons(h, ex03_02_Tail.tail(l))
}

object ex03_04_Drop {
  @tailrec
  def drop[A](l: BookList[A], n: Int): BookList[A] = {
    if (n==0) l
    else drop(ex03_02_Tail.tail(l), n-1)
  }
}

object ex03_05_DropWhile {
  @tailrec
  def dropWhile[A](l: BookList[A])(p: A => Boolean): BookList[A] = l match {
    case Cons(h, t) if p(h) => dropWhile(t)(p)
    case _ => l
  }
}

object ex03_06_Init {
  def initSlow[A](l: BookList[A]): BookList[A] = l match {
    case Cons(h, Cons(_, Nil)) => Cons(h, Nil)
    case Cons(h, t) => Cons(h, init(t))
    case Nil => Nil
  }

  // solution uses listbuffer - more efficient, but needs sideeffects
  def init[A](l: BookList[A]): BookList[A] = {
    @tailrec
    def loop(list: BookList[A], acc: BookList[A]): BookList[A] = list match {
      case Nil => acc
      case Cons(h, Cons(m, Nil)) => loop(Nil, Cons(h, acc))
      case Cons(h, t) => loop(t, Cons(h,acc))
    }

    @tailrec
    def reverse(toRev: BookList[A], acc: BookList[A]): BookList[A] = toRev match {
      case Nil => acc
      case Cons(h, t) => reverse(t, Cons(h, acc))
    }

    reverse(loop(l, Nil), Nil)
  }
}

object ex03_07_FoldRShortcircuitProduct {
  // does not work, because foldRight recurses before f is applied

  // non-FP
  // note that this only saves calls of f ('*' here),
  // but still recurses through the whole list
  def productUgly(l: BookList[Int]): Int = {
    object Annihilation extends Exception

    def multOrFail(a: Int, b: Int): Int = {
      if (a == 0 || b == 0) throw Annihilation
      else a * b
    }

    try {
      BookList.foldRight(l, 1)(multOrFail)
    } catch {
      case Annihilation => 0
    }
  }
}

object ex03_08_NilConsFoldR {
  val tryThis: BookList[Int] = BookList.foldRight(Cons(1,Cons(2,Cons(3,Nil))), Nil:BookList[Int])(Cons(_,_))
}

object ex03_09_LengthFold {
  def length[A](as: BookList[A]): Int = {
    BookList.foldRight(as, 0){case (_, n:Int) => n + 1}
  }
}

object ex03_10_FoldLeft {
  @tailrec
  def foldLeft[A,B](as: BookList[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }
}

object ex03_11_FoldLeftUsage {
  def sum(as: BookList[Int]) = ex03_10_FoldLeft.foldLeft(as, 0)(_ + _)
  def product(as: BookList[Int]) = ex03_10_FoldLeft.foldLeft(as, 1)(_ * _)
  def length[A](as: BookList[A]) = ex03_10_FoldLeft.foldLeft(as, 0)((n, _) => n + 1)
}

object ex03_12_Reverse {
  def reverse[A](as: BookList[A]) = ex03_10_FoldLeft.foldLeft(as, Nil:BookList[A]){(acc, l) => Cons(l, acc)}
}

object ex03_13_FoldInTermsOfEachother {
  def foldRight[A,B](as: BookList[A], z: B)(f: (A,B) => B) = {
    ex03_10_FoldLeft.foldLeft(ex03_12_Reverse.reverse(as), z)((b,a) => f(a,b))
  }

  def foldLeft[A,B](as: BookList[A], z: B)(f: (B,A) => B) = {
    // there's no smart way according to the solutions
    // not only does it need 2*n steps, it also is not stack-safe
  }
}

object ex03_14_Append {
  def apply[A](as: BookList[A], bs: BookList[A]): BookList[A] = {
    ex03_13_FoldInTermsOfEachother.foldRight(as, bs)(Cons(_, _))
  }
}

object ex03_15_Concat {
  def apply[A](ls: BookList[BookList[A]]): BookList[A] = {
    import ex03_13_FoldInTermsOfEachother.{foldRight => foldR}
    import ex03_14_Append.{apply => append}
    foldR(ls, Nil: BookList[A])(append)
  }
}

object ex03_16_MapPlus1 {
  def apply(ls: BookList[Int]): BookList[Int] = {
    import ex03_13_FoldInTermsOfEachother.{foldRight => foldR}
    foldR(ls, Nil: BookList[Int])((n, l) => Cons(n+1, l))
  }
}

object ex03_17_MapToString {
  // ommited, same as ex03_16
}

object ex03_18_Map {
  def apply[A, B](ls: BookList[A])(f: A => B): BookList[B] = {
    val buf = collection.mutable.ListBuffer[B]()

    @tailrec
    def loop(l: BookList[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => {
        buf += f(h)
        loop(t)
      }
    }
    loop(ls)
    BookList(buf.toList: _*)
  }
}

object ex03_19_Filter {
  def apply[A](l: BookList[A])(p: A => Boolean): BookList[A] = {
    // same problem as Map, this time using foldR (no specific reason)
    import ex03_13_FoldInTermsOfEachother.{foldRight => foldR}
    foldR(l, Nil: BookList[A]){(e, newlist) => if (p(e)) Cons(e, newlist) else newlist}
  }
}

object ex03_20_FlatMap {
  def apply[A, B](l: BookList[A])(f: A => BookList[B]): BookList[B] = {
    import ex03_13_FoldInTermsOfEachother.{foldRight => foldR}
    import ex03_14_Append.{apply => append}

    foldR(l, Nil: BookList[B]){(e, res) => append(f(e), res)}
  }
}

object ex03_21_FilterUsingFlatMap {
  def apply[A](l: BookList[A])(p: A => Boolean): BookList[A] = {
    import ex03_20_FlatMap.{apply => flatMap}
    flatMap(l)(e => if (p(e)) BookList(e) else Nil)
  }
}

object ex03_22_ZipAdd {
  // using zip
  def apply(as: BookList[Int], bs: BookList[Int]): BookList[Int] = {
    ex03_23_ZipWith(as, bs)(_ + _)
  }
}

object ex03_23_ZipWith {
  def apply[A, B, C](as: BookList[A], bs: BookList[B])(f: (A, B) => C): BookList[C] = {
    /*
    using the ListBuffer-method would be smarter here, because then it
    would be possible to use ZipWith even if one List has an infinite length
    also, we would not have to process the last elements of one list if they
    are after the end of the other list
     */

    /*
    this is not only inefficient, its wrong!
    it uses the end of the longer list, instead of its start

    import ex03_12_Reverse.reverse
    val asR = reverse(as)
    val bsR = reverse(bs)

    @tailrec
    def loop(xs: List[A], ys: List[B], acc: List[C]): List[C] = (xs,ys) match {
      case (Nil, _) => acc
      case (_, Nil) => acc
      case (Cons(x, xs), Cons(y, ys)) => loop(xs, ys, Cons(f(x,y), acc))
    }
    loop(asR, bsR, Nil: List[C])
    */

    val buf = collection.mutable.ListBuffer[C]()

    @tailrec
    def loop(xs: BookList[A], ys:BookList[B]): Unit = (xs, ys) match {
      case (Nil, _) => ()
      case (_, Nil) => ()
      case (Cons(x, xs), Cons(y, ys)) => {
        buf += f(x,y)
        loop(xs, ys)
      }
    }
    loop(as, bs)
    BookList(buf: _*)
  }
}