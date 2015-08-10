package ch.pascalschwarz.manningfpinscala.Chap07FunctionalParallelism.NonBlocking

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{CountDownLatch, ExecutorService}

sealed trait Future[A] {
  private[Chap07FunctionalParallelism] def apply(k: A => Unit): Unit
}
object Par {
  type Par[+A] = ExecutorService => Future[A]
  def run[A](es: ExecutorService)(p:Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) {a => ref.set(a); latch.countDown()}
    latch.await()
    ref.get
  }
}
