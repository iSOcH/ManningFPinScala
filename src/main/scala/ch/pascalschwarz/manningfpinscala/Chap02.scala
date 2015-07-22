package ch.pascalschwarz.manningfpinscala

import scala.annotation.tailrec


import scala.annotation.tailrec

object ex02_01_FibRec {
  def fib(n: Int): Int = {
    @tailrec
    def go(n:Int, prev:Int, cur:Int): Int = {
      if (n==0) prev
      else go(n-1, cur, prev+cur)
    }
    go(n,0,1)
  }
}

object ex02_02_SortedHOF {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def loop(curIdx: Int, nextIdx: Int): Boolean = {
      if (nextIdx >= as.length) true
      else if (ordered(as(curIdx), as(nextIdx))) loop(nextIdx, nextIdx + 1)
      else false
    }
    loop(0,1)
  }
}

object ex02_03_Curry {
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a => f(a, _) // shorter for: a => b => f(a, b)
  }
}

object ex02_04_UnCurry {
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }
}

object ex02_05_Compose {
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}