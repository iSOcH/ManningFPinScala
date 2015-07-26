package ch.pascalschwarz.manningfpinscala

sealed trait BEither[+E, +A] {
  // ex04_06
  def map[B](f: A => B): BEither[E, B] = this match {
    case BRight(v) => BRight(f(v))
    case BLeft(e) => BLeft(e)
  }

  def orElse[B >: A, E2 >: E](b: => BEither[E2, B]): BEither[E2, B] = this match {
    case BLeft(_) => b
    case _ => this
  }

  def flatMap[B, E2 >: E](f: A => BEither[E2, B]) = {
    map(f) match {
      case BRight(i) => i
      case BLeft(e) => BLeft(e)
    }
  }

  def map2[B, C, E2 >: E](b: BEither[E2, B])(f: (A, B) => C): BEither[E2, C] = (this, b) match {
    case (BRight(aa), BRight(bb)) => BRight(f(aa, bb))
    case (BLeft(e), _) => BLeft(e)
    case (_, BLeft(e)) => BLeft(e)
  }

  def map2ForComp[B, C, E2 >: E](b: BEither[E2, B])(f: (A, B) => C): BEither[E2, C] = {
    // no need to care about Lefts...
    for {
      aa <- this
      bb <- b
    } yield f(aa,bb)
  }
}
case class BLeft[+E](value: E) extends BEither[E, Nothing]
case class BRight[+A](value: A) extends BEither[Nothing, A]

object ex04_07 {
  def traverse[E, A, B](as: List[A])(f: A => BEither[E, B]): BEither[E, List[B]] = as match {
    case Nil => BRight(Nil)
    case a :: t => f(a).map2(traverse(t)(f))(_ :: _)
  }

  def sequence[E, A](es: List[BEither[E, A]]): BEither[E, List[A]] = {
    traverse(es)(x => x)
  }
}