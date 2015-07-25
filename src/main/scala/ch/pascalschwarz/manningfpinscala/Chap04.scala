package ch.pascalschwarz.manningfpinscala

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    // i really dont like the solution from the book, patternmatching is more readable here
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = if (map(f) getOrElse false) this else None
}
case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]

