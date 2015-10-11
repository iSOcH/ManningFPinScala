package ch.pascalschwarz.manningfpinscala.Chap08Testing

import java.util.concurrent.Executors

import ch.pascalschwarz.manningfpinscala.Chap05Streams.Stream
import ch.pascalschwarz.manningfpinscala.Chap06FunctionalState.{SimpleRNG, State, RNG}
import ch.pascalschwarz.manningfpinscala.Chap07FunctionalParallelism.NonBlocking.NonBlocking.Par
import ch.pascalschwarz.manningfpinscala.Chap08Testing.Prop.{MaxSize, TestCases, SuccessCount, FailedCase}

trait Prop1 {
  def check: Boolean

  // ex08_03
  def &&(p: Prop1): Prop1 = new Prop1 { def check = this.check && p.check }
}

object Prop {
  type FailedCase = String
  type TestCases = Int
  type SuccessCount = Int
  type MaxSize = Int

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    randomStream(as)(rng).zip(Stream.from(0)).take(n).map{ case (a,i) => try {
      if (f(a)) Passed else Falsified(a.toString, i)
    } catch { case e: Exception => Falsified(buildMsg(a, e), i)
    }}.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)
  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop{ (max, n, rng) =>
    val casesPerSize = (n + (max - 1)) / max
    val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
    val prop: Prop = props.map(p => Prop { (max, _, r) =>
      p.run(max, casesPerSize, r)
    }).toList.reduce(_ && _)
    prop.run(max, n, rng)
  }

  val S = Gen.weighted(
    Gen.choose(1,4).map(Executors.newFixedThreadPool) -> 0.75,
    Gen.unit(Executors.newCachedThreadPool) -> 0.25
  )
  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) {case (s,a) => Par.run(s)(f(a))}

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }
  def checkPar(p: Par[Boolean]): Prop = forAllPar(Gen.unit(()))(_ => p)

  def buildMsg[A](s: A, e: Exception): String =
    s"test case:\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = SimpleRNG(System.currentTimeMillis())): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falisified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }
}
case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  // ex08_09
  def &&(p: Prop): Prop = Prop{ (max, n, rng) =>
    Stream(this.labelled("left"), p.labelled("right")).map(_.run(max, n, rng)).find(_.isFalsified).getOrElse(Passed)
  }
  def ||(p: Prop): Prop = Prop{ (max, n, rng) =>
    run(max, n, rng) match {
      case Falsified(_, _) => p.run(max, n, rng)
      case x => x
    }
  }
  def labelled(s: String): Prop = Prop { (max, n,rng) =>
    run(max, n,rng) match {
      case Falsified(e, c) => Falsified(s + "\n" + e, c)
      case success => success
    }
  }
}

sealed trait Result {
  def isFalsified : Boolean
}
case object Proved extends Result {
  override val isFalsified = false
}
case object Passed extends Result {
  override val isFalsified = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override val isFalsified = true
}

case class Gen[+A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] = Gen{ sample map f }

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def **[B](g: Gen[B]): Gen[(A,B)] = (this map2 g)(_ -> _)

  // ex08_06
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen{ sample.flatMap(f.andThen(_.sample)) }
  def listOfN(size: Int) = Gen.listOfN(size, this)
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(listOfN)

  // ex08_10
  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  // ex08_04
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  // ex08_05
  def unit[A](a: => A): Gen[A] = Gen(State(r => (a,r)))
  def boolean: Gen[Boolean] = Gen(State {RNG.boolean})
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen{
    State.sequence(List.fill(n)(g.sample))
  }

  // play
  def choosePair(start: Int, stopExclusive: Int): Gen[(Int,Int)] = Gen(State{r =>
    val (first, r2) = choose(start, stopExclusive).sample.run(r)
    val (second, r3) = choose(start, stopExclusive).sample.run(r2)
    ((first, second), r3)
  })
  def toOption[A](g: Gen[A]): Gen[Option[A]] = Gen(g.sample.map(x => Some(x)))
  def fromOption[A](g: Gen[Option[A]]): Gen[A] = Gen(g.sample.filter(_.isDefined).map(_.get)) // does not terminate if all of the generated Options are None

  // ex08_07
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

  // ex 08_08
  def double: Gen[Double] = Gen(State(RNG.double))
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val (p1, p2) = (Math.max(0, g1._2), Math.max(0, g2._2)) // probabilities cannot be <0
    Gen.double.flatMap{d =>
      val first = d * (p1+p2) < p1
      if (first) g1._1 else g2._1
    }
  }
}

case class SGen[+A](forSize: Int => Gen[A]) {
  // ex08_11
  def apply(n: Int): Gen[A] = forSize(n)
  def map[B](f: A => B): SGen[B] = SGen{ forSize andThen(_ map f) }
  def flatMap[B](f: A => Gen[B]): SGen[B] = SGen{ forSize andThen(_ flatMap f) }
}

object SGen{
  // ex08_12
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen{ g.listOfN }

  // ex08_13
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen{ n => g.listOfN(n max 1) } // there will be more testCases of size 1 this way... not that important though
}

object SomeProps {
  import Prop._
  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(SGen.listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  // ex08_14
  val sortedProp = forAll(SGen.listOf(smallInt)) { ns =>
    val sorted = ns.sorted
    (for {
      i <- sorted.indices if i > 0
      (h, t) = sorted.splitAt(i)
    } yield h.last <= t.min).fold(true)(_ && _) &&
      sorted.forall(ns.contains) && ns.forall(sorted.contains)
  }

  val p2 = check {
    val ES = Executors.newCachedThreadPool()
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    Par.run(ES)(p) == Par.run(ES)(p2)
  }
  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p, p2)(_ == _)
  val p3 = check {
    val ES = Executors.newCachedThreadPool()
    Par.run(ES)(equal(Par.map(Par.unit(1))(_ + 1), Par.unit(2)))
  }
  val p2_2 = checkPar {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }

  val pint = Gen.choose(0,10) map (Par.unit)
  val p4 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

  // ex08_16
  val pint2: Gen[Par[Int]] = Gen.choose(-100,100).listOfN(Gen.choose(0,20)).map(l =>
    l.foldLeft(Par.unit(0)) { (p, i) =>
      val ui = Par.unit(i)
      val calc = () => Par.map2(p, ui)(_ + _)
      if (i % 2 == 0) Par.fork { calc() }
      else calc()
    }
  )
  val p4_2 = forAllPar(pint2)(n => equal(Par.map(n)(y => y), n))

  // ex08_17
  val forkProp: Prop = Prop.forAllPar(pint2){ i =>
    equal(Par.fork(i), i)
  }

  /* ex08_18
    s.takeWhile(_ => true) == s
    s.takeWhile(_ => false) == List()
    s.takeWhile(f) ++ s.dropWhile(f) == s
   */
}