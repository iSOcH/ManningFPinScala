package ch.pascalschwarz.manningfpinscala.Chap06FunctionalState

case class State[S, +A](run: S => (A, S)) {
  // ex 06_10
  def map[B](f: A => B): State[S, B] = State { s =>
    val (a, s2) = run(s)
    (f(a), s2)
  }
  def map2[B,C](sb: State[S, B])(f: (A,B) => C): State[S, C] = State { s =>
    val (a, s2) = run(s)
    val (b, s3) = sb.run(s2)
    (f(a, b), s3)
  }
  def flatMap[B](f: A => State[S, B]): State[S, B] = State{ s =>
    val (a, s2) = run(s)
    f(a).run(s2)
  }
}

object State {
  // ex 06_10
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] = State { s =>
    ss.foldRight{ (List[A](),s) } { case (sa, (acc, state)) =>
      val (a, nextState) = sa.run(state)
      (a :: acc, nextState)
    }
  }
}