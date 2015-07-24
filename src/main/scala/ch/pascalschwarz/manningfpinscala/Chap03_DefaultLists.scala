package ch.pascalschwarz.manningfpinscala

import scala.annotation.tailrec

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

    // checking length is expensive :/
    @tailrec
    def loop(sup: List[A]): Boolean = sup match {
      case l if l.length >= sub.length => if ({zipWith(l, sub)(_ == _)}.forall(_ == true)) true else loop(l.tail)
      case _ => false
    }
    loop(sup)
  }
}
