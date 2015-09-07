package ch.pascalschwarz.manningfpinscala.Chap08Testing

import ch.pascalschwarz.manningfpinscala.Chap05Streams.Stream
import ch.pascalschwarz.manningfpinscala.Chap06FunctionalState.{State, RNG}
import ch.pascalschwarz.manningfpinscala.Chap08Testing.Prop.{TestCases, SuccessCount, FailedCase}

trait Prop1 {
  def check: Boolean

  // ex08_03
  def &&(p: Prop1): Prop1 = new Prop1 { def check = this.check && p.check }
}



object Prop {
  type FailedCase = String
  type TestCases = Int
  type SuccessCount = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(as)(rng).zip(Stream.from(0)).take(n).map{ case (a,i) => try {
      if (f(a)) Passed else Falsified(a.toString, i)
    } catch { case e: Exception => Falsified(buildMsg(a, e), i)
    }}.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  def buildMsg[A](s: A, e: Exception): String =
    s"test case:\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}
case class Prop(run: (TestCases,RNG) => Result) {
  // ex08_09
  def &&(p: Prop): Prop = Prop{ (n, rng) =>
    Stream(this.labelled("left"), p.labelled("right")).map(_.run(n,rng)).find(_.isFalsified).getOrElse(Passed)
  }
  def ||(p: Prop): Prop = Prop{ (n, rng) =>
    run(n, rng) match {
      case Falsified => p.run(n, rng)
      case x => x
    }
  }
  def labelled(s: String): Prop = Prop { (n,rng) =>
    run(n,rng) match {
      case Falsified(e, c) => Falsified(s + "\n" + e, c)
      case success => success
    }
  }
}

sealed trait Result {
  def isFalsified : Boolean
}
case object Passed extends Result {
  override val isFalsified = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override val isFalsified = true
}

case class Gen[A](sample: State[RNG,A]) {
  // ex08_06
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen{ sample.flatMap(f.andThen(_.sample)) }
  def listOfN(size: Int) = Gen.listOfN(size, this)
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(listOfN)
}

object Gen {
  // ex08_04
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State{r =>
    val rangeScale = (stopExclusive - start) / Math.pow(2,32)
    val (n, nextrng) = r.nextInt
    ((n * rangeScale).toInt + start, nextrng)
  })

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