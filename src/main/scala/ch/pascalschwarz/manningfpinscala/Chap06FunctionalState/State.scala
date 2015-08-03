package ch.pascalschwarz.manningfpinscala.Chap06FunctionalState

import ch.pascalschwarz.manningfpinscala.Chap06FunctionalState.RNG.Rand

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
  lazy val int: Rand[Int] = _.nextInt
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0xDEECE66DL + 0xBL) & 0xFFFFFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, r2) = s(rng)
    (f(a), r2)
  }

  // ex06_01
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt
    (n & 0x7FFFFFFF, nextRNG) // if first bit is 0, int is >=0
  }

  // ex06_02
  def double(rng: RNG): (Double, RNG) = {
    val (i, nextRNG) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), nextRNG)
  }

  // ex06_03
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i,d), rng3)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i,d), rng2) = intDouble(rng)
    ((d,i), rng2)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1,d2,d3), rng4)
  }

  // ex06_04
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(n: Int, acc: List[Int])(r: RNG): (List[Int], RNG) = {
      if (n == 0) (acc, r)
      else {
        val (i, nRNG) = r.nextInt
        loop(n-1, i :: acc)(nRNG)
      }
    }
    loop(count, List())(rng)
  }

  // ex06_05
  def double2: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // ex06_06
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a,b), rng3)
  }

  // ex06_07
  // sequenceSol and also sequenceSol2 are both not stacksafe
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = initialRng => {
    // foldRight on default List is stacksafe
    fs.foldLeft{ (List[A](), initialRng) } { case ((acc, rng), rand) => {
      val (res, newRng) = rand(rng)
      (res :: acc, newRng)
    }}
  }
  def sequenceSol[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]())){ (f, acc) => map2(f, acc)(_ :: _) }
  }
  def sequenceSol2[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldLeft(unit(List[A]())){ (acc, f) => map2(f, acc)(_ :: _) }
    // as long as we're talking about random values, there is no need to reverse
  }
  def ints2(n: Int): Rand[List[Int]] = { // also possible = rng => {
    sequence{ List.fill(n) (nonNegativeInt _) } // then here: (rng)
  }
}