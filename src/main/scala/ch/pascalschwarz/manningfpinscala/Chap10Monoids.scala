package ch.pascalschwarz.manningfpinscala

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  // ex 10_05
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    // iterates twice
    //val mapped = as map f
    //concatenate(mapped, m)

    as.foldLeft(m.zero)((b: B, a: A) => m.op(b, f(a)))
  }

  // ex10_06
  def foldLeftViaFoldMap[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    // endoMonoid[B]'s op is (B => B) => (B => B) => (B => B)
    // we change the order of f's arguments so that f: A => B => B
    // the a is supplied in foldMap which leaves B => B, endoMonoid[B].op
    // can then be used to combine these into a single B => B - which is
    // finally resolved to a single B by application to `z`
    foldMap(as, MonoidInstances.endoMonoid[B])(a => b => f(b, a))(z)
  }

  // ex10_07
  def foldMap[A,B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.length == 0) m.zero
    else if (as.length == 1) f(as.head)
    else {
      val (a1, a2) = as.splitAt(as.length / 2)
      m.op(foldMap(a1, m)(f), foldMap(a2, m)(f))
    }
  }

  // ex10_08
  import ch.pascalschwarz.manningfpinscala.Chap07FunctionalParallelism.NonBlocking.NonBlocking._
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
    val zero: Par[A] = Par.unit(m.zero)
  }
  def foldMapPar[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    foldMap(v, par(m))(a => Par.lazyUnit(f(a)))
  }

  // ex10_09
  // accepts ascending and descending sequences
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    // min, max, isOrdered, isAscending
    val m = new Monoid[Option[(Int, Int, Boolean, Option[Boolean])]] {
      override def op(a1: Option[(Int, Int, Boolean, Option[Boolean])], a2: Option[(Int, Int, Boolean, Option[Boolean])]) = {
        val dir1 = a1.flatMap(_._4)
        val dir2 = a2.flatMap(_._4)
        val dir = dir1.orElse(dir2)
        val maybeSorted = if (dir1.isDefined && dir2.isDefined) dir1 == dir2 else true

        (a1, a2) match {
          case (Some((min1, max1, ordered1, _)), Some((min2, max2, ordered2, _))) if !maybeSorted =>
            Some((min1 min min2, max1 max max2, false, None))
          case (Some((min1, max1, ordered1, _)), Some((min2, max2, ordered2, _))) if dir.isDefined => {
            val sorted = if (dir.get) max1 <= min2 else max2 <= min1
            Some((min1 min min2, max1 max max2, ordered1 && ordered2 && sorted, dir))
          }
          case (Some((min1, max1, ordered1, _)), Some((min2, max2, ordered2, _))) =>
            Some((min1 min min2, max1 max max2, ordered1 && ordered2 && (max1 <= min2 || min1 >= max2), if ((max1 == min2) || (min1 == max2)) None else Some(max1 < min2)))
          case (x, None) => x
          case (None, x) => x
        }
      }

      val zero = None
    }

    foldMap(ints, m)(i => Some(i, i, true, None)).map(_._3).getOrElse(true)
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // ex10_11
  def wordCount(input: String): Int = {
    // wc produces Stubs and Parts that never contain a whitespace
    def wc(c: Char): WC = {
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString)
    }

    def unstub(s: String): Int = s.length min 1

    foldMap(input.to[IndexedSeq], MonoidInstances.wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, c, r) => unstub(l) + c + unstub(r)
    }
  }

  // ex10_18
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    import MonoidInstances.{mapMergeMonoid, intAddition}
    val M = mapMergeMonoid[A,Int](intAddition)
    foldMap(as, M)(v => Map(v -> 1))
  }
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

  def mapMergeMonoid[K,V](V: Monoid[V]) = new Monoid[Map[K,V]] {
    val zero = Map[K,V]()
    def op(a: Map[K, V], b: Map[K, V]) = (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
      acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
    }
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

  // ex10_10
  import ch.pascalschwarz.manningfpinscala.Monoid.{Part, Stub, WC}
  val wcMonoid = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
      case (Stub(s1), Part(p1, c, p2)) => Part(s1 + p1, c, p2)
      case (Part(p1, c, p2), Stub(s2)) => Part(p1, c, p2 + s2)
      case (Part(p1, c1, p2), Part(p3, c2, p4)) =>
        Part(p1, c1 + (if ((p2 + p3).isEmpty) 0 else 1) + c2, p4)
    }

    val zero: WC = Stub("")
  }

  // ex10_16
  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]) = new Monoid[(A, B)] {
    val zero: (A, B) = (A.zero, B.zero)
    def op(a1: (A, B), a2: (A, B)) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
  }

  // ex10_17
  def functionMonoid[A,B](B: Monoid[B]) = new Monoid[A=>B] {
    val zero: (A) => B = _ => B.zero
    def op(f1: (A) => B, f2: (A) => B): (A) => B = a => B.op(f1(a), f2(a))
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


    // endofunctions: equality on functions is not useful here, thats why the test fails
    val fns: List[Int => Int] = List(
      (x: Int) => x + 15,
      (x: Int) => x - 13,
      (x: Int) => x * 12,
      (x: Int) => x / 11
    )
    val fnGens:List[Gen[Int => Int]] = fns.map(fn => Gen.unit(fn))
    val fnGen:Gen[Int => Int] = fnGens.reduce(Gen.union[Int => Int])
    val endo = monoidLaws(MonoidInstances.endoMonoid[Int], fnGen) labelled "endofunctions"

    Prop.run(string && intAddition && intMultiplication && booleanOr && booleanAnd && option/* && endo*/, 200, 200)
  }
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  // ex10_15
  def toList[A](fa: F[A]): List[A] = foldRight(fa)(List[A]())(_ :: _)
}

// ex10_13
object ListFoldable extends Foldable[List] {
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero)((b:B, a:A) => mb.op(b, f(a)))
}
object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]): B = Monoid.foldMap(as, mb)(f)
}
object StreamFoldable extends Foldable[Stream] {
  def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  def foldMap[A, B](as: Stream[A])(f: (A) => B)(mb: Monoid[B]): B = foldRight(as)(mb.zero)((a:A, b:B) => mb.op(f(a), b))
}
object TreeFoldable extends Foldable[Tree] {
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }
}

// ex10_14
object OptionFoldable extends Foldable[Option] {
  def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as.map(a => f(a, z)).getOrElse(z)
  def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as.map(a => f(z, a)).getOrElse(z)
  def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B = as.map(f).getOrElse(mb.zero)
}