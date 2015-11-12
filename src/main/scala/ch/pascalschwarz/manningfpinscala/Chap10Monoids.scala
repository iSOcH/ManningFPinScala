package ch.pascalschwarz.manningfpinscala

import ch.pascalschwarz.manningfpinscala.Chap06FunctionalState.SimpleRNG

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object MonoidInstances {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(l1: List[A], l2: List[A]) = l1 ++ l2
    val zero = Nil
  }

  // ex10_01
  val intAddition = new Monoid[Int] {
    def op(i1: Int, i2: Int) = i1 + i2
    val zero = 0
  }
  val intMultiplication = new Monoid[Int] {
    def op(i1: Int, i2: Int) = i1 * i2
    def zero: Int = 1
  }
  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    def zero: Boolean = false
  }
  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = true
  }

  // ex10_02
  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    val zero: Option[A] = None
  }

  // ex10_03
  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 andThen a2 // a2 andThen a1 also possible, dual Monoid
    val zero: A => A = identity
  }
}

object MonoidLaws {
  import ch.pascalschwarz.manningfpinscala.Chap08Testing._

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = Prop.forAll(gen.listOfN(3)) { case List(a1, a2, a3) =>
    import m._
    val zeroLaw = op(a1, zero) == a1 && op(zero, a1) == op(a1, zero)
    val commLaw = op(op(a1, a2), a3) == op(a1, op(a2, a3))

    zeroLaw && commLaw
  }

  def runTests: Unit = {
    val intGen = Gen.choose(Int.MinValue, Int.MaxValue)

    val string = monoidLaws(MonoidInstances.stringMonoid, intGen.flatMap(n => Gen.stringN(n)))

    val intAddition = monoidLaws(MonoidInstances.intAddition, intGen) labelled "intAddition"
    val intMultiplication = monoidLaws(MonoidInstances.intMultiplication, intGen) labelled "intMultiplication"
    val booleanOr = monoidLaws(MonoidInstances.booleanOr, Gen.boolean) labelled "booleanOr"
    val booleanAnd = monoidLaws(MonoidInstances.booleanAnd, Gen.boolean) labelled "booleanAnd"

    val option = monoidLaws(MonoidInstances.optionMonoid[Int], Gen.toOption(intGen)) labelled "option"

//    val fns = List(
//      (x: Int, y: Int) => x + y,
//      (x: Int, y: Int) => x - y,
//      (x: Int, y: Int) => x * y,
//      (x: Int, y: Int) => x / y//,
//      //MonoidInstances.intAddition.op
//    )
//    val endo = monoidLaws(MonoidInstances.endoMonoid)

    Prop.run(string && intAddition && intMultiplication && booleanOr && booleanAnd && option, 200, 200)
  }
}