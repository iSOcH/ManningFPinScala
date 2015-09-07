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
  def boolean: Gen[Boolean] = Gen(State {RNG.boolean})
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen{
    State.sequence(List.fill(n)(g.sample))
  }

  // free
  def choosePair(start: Int, stopExclusive: Int): Gen[(Int,Int)] = Gen(State{r =>
    val (first, r2) = choose(start, stopExclusive).sample.run(r)
    val (second, r3) = choose(start, stopExclusive).sample.run(r2)
    ((first, second), r3)
  })
  def toOption[A](g: Gen[A]): Gen[Option[A]] = Gen(g.sample.map(x => Some(x)))
  def fromOption[A](g: Gen[Option[A]]): Gen[A] = Gen(g.sample.filter(_.isDefined).map(_.get)) // does not terminate if all of the generated Options are None
}