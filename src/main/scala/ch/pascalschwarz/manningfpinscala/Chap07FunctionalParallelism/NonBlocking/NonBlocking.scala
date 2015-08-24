package ch.pascalschwarz.manningfpinscala.Chap07FunctionalParallelism.NonBlocking

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

object NonBlocking {
  trait Future[+A] {
    private[Chap07FunctionalParallelism] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new java.util.concurrent.atomic.AtomicReference[A] // A mutable, threadsafe reference, to use for storing the result
      val latch = new CountDownLatch(1) // A latch which, when decremented, implies that `ref` has the result
      p(es) { a => ref.set(a); latch.countDown() } // Asynchronously set the result, and decrement the latch
      latch.await() // Block until the `latch.countDown` is invoked asynchronously
      ref.get // Once we've passed the latch, we know `ref` has been set, and return its value
    }

    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    /** A non-strict version of `unit` */
    def delay[A](a: => A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    def fork[A](a: => Par[A]): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
      }

    /**
     * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
     * This will come in handy in Chapter 13.
     */
    def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
      def apply(k: A => Unit) = f(k)
    }

    /**
     * Helper function, for evaluating an action
     * asynchronously, using the given `ExecutorService`.
     */
    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })

    def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] =
      es => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None
          // this implementation is a little too liberal in forking of threads -
          // it forks a new logical thread for the actor and for stack-safety,
          // forks evaluation of the callback `cb`
          val combiner = Actor[Either[A,B]](es) {
            case Left(a) =>
              if (br.isDefined) eval(es)(cb(f(a,br.get)))
              else ar = Some(a)
            case Right(b) =>
              if (ar.isDefined) eval(es)(cb(f(ar.get,b)))
              else br = Some(b)
          }
          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

    // specialized version of `map`
    def map[A,B](p: Par[A])(f: A => B): Par[B] =
      es => new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => eval(es) { cb(f(a)) })
      }

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A,B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil => unit(Nil)
        case h :: t => map2(h, fork(sequence(t)))(_ :: _)
      }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] =
      sequence(as.map(asyncF(f)))

    def parMap[A,B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
      sequenceBalanced(as.map(asyncF(f)))

    // ex 07_11
    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => new Future[A] {
      override def apply(cb: A => Unit) = n(es){ index => eval(es) {choices(index)(es)(cb)} }
    }
    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = choiceN{
      map(cond){res => if (res) 0 else 1}
    }(List(t,f))

    // ex07_12
    def choiceMap[K,V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es => new Future[V] {
      override def apply(cb: V => Unit) = key(es){ k => eval(es) {choices(k)(es)(cb)} }
    }

    // ex07_13
    def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => new Future[B] {
      override def apply(cb: B => Unit) = pa(es)(a => choices(a)(es)(cb))
    }
    def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = chooser(n)(choices)
    def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = chooser(cond)(Map(true -> t, false -> f))

    def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = chooser(a)(f)

    // ex07_14
    def join[A](a: Par[Par[A]]): Par[A] = es => new Future[A] {
      override def apply(cb: A => Unit): Unit = a(es)(innerA => eval(es) {innerA(es)(cb)})
    }
    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(x => x)
    def flatMapViaJoin[A,B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))
  }
}
