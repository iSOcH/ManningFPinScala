package ch.pascalschwarz.manningfpinscala.Chap08Testing

import ch.pascalschwarz.manningfpinscala.Chap06FunctionalState.{State, RNG}
import ch.pascalschwarz.manningfpinscala.Chap08Testing.Prop.{SuccessCount, FailedCase}

trait Prop1 {
  def check: Boolean

  // ex08_03
  def &&(p: Prop1): Prop1 = new Prop1 { def check = this.check && this.check }
}



object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

case class Gen[A](sample: State[RNG,A])
object Gen {
  // ex08_04
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State{r =>
    val rangeScale = (stopExclusive - start) / Math.pow(2,32)
    val (n, nextrng) = r.nextInt
    ((n * rangeScale).toInt + start, nextrng)
  })

  // ex08_05
  def unit[A](a: => A): Gen[A] = Gen(State(r => (a,r)))
  def boolean: Gen[Boolean] = Gen(State { r =>
    val (n, nextrng) = r.nextInt
    (n % 2 == 0, nextrng)
  })
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen{
    State.sequence(List.fill(n)(g.sample))
  }
}