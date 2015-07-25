package ch.pascalschwarz.manningfpinscala

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object ex03_24_HasSubsequence {
  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] = {
    val buf = collection.mutable.ListBuffer[C]()

    @tailrec
    def loop(xs: List[A], ys:List[B]): Unit = (xs, ys) match {
      case (List(), _) => ()
      case (_, List()) => ()
      case (x :: xs, y :: ys) => {
        buf += f(x,y)
        loop(xs, ys)
      }
    }
    loop(as, bs)
    buf.toList
  }

  def apply[A](sup: List[A], sub:List[A]): Boolean = {
    // very simple but not really efficient

    val subLen = sub.length
    val supLen = sup.length

    @tailrec
    def loop(sup: List[A], supLen: Int): Boolean = sup match {
      case l if supLen >= subLen => if ({zipWith(l, sub)(_ == _)}.forall(_ == true)) true else loop(l.tail, supLen - 1)
      case _ => false
    }
    loop(sup, supLen)
  }
}

object ex03_25_TreeSize {
  def apply[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + apply(l) + apply(r)
  }
}

object ex03_26_TreeMax {
  def apply(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(l, r) => apply(l) max apply(r)
  }
}

object ex03_27_depth {
  def apply[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (apply(l) max apply(r))
  }
}

object ex03_28_Map {
  def apply[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(apply(l)(f), apply(r)(f))
  }
}

object ex03_29_TreeFold {
  def apply[A, B](tree: Tree[A])(conv: A => B)(comb: (B,B) => B): B = tree match {
    case Leaf(x) => conv(x)
    case Branch(l,r) => {
      val fold: Tree[A] => B = t => apply(t)(conv)(comb)
      comb(fold(l), fold(r))
    }
  }

  def fold[A, B](tree: Tree[A])(conv: A => B)(comb: (B,B) => B) = apply(tree)(conv)(comb)
  def size[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)
  def max(t: Tree[Int]): Int = fold(t)(x => x)(_ max _)
  def depth[A](t: Tree[A]): Int = fold(t)(_ => 0)((l,r) => 1 + (l max r))
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t){v => Leaf(f(v)): Tree[B]}{Branch(_,_)}
}