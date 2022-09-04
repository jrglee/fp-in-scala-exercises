package jrglee.fp.exercises
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.TimeoutException
import scala.util.Try

object Chapter07 {

  object Section1 {
    type Par[A]

    object Par {
      def map2[A, B, C](pa: => Par[A], pb: => Par[B])(f: (A, B) => C): Par[C] = ???
    }
  }

  object Section2 {
    trait Par[A]

    object Par {
      def unit[A](a: A): Par[A] = ???
      def map2[A, B, C](pa: => Par[A], pb: => Par[B])(f: (A, B) => C): Par[C] = ???
      def fork[A](a: => Par[A]): Par[A] = ???
      def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
      def run[A](a: Par[A]): A = ???
    }
  }

  object Section3 {
    import java.util.concurrent.{Callable, ExecutorService, Future}

    type Par[A] = ExecutorService => Future[A]

    object Par {
      def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

      private case class UnitFuture[A](get: A) extends Future[A] {
        override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

        override def isCancelled: Boolean = false

        override def isDone: Boolean = true

        override def get(timeout: Long, unit: TimeUnit): A = get
      }

      def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }

      def fork[A](a: => Par[A]): Par[A] = es =>
        es.submit(new Callable[A] {
          override def call(): A = a(es).get
        })

      def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

      def map2WithTimeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)

        new Future[C] {
          private val result = new AtomicReference[Try[C]]()

          private def writeResultIfDone(): Unit = if (isDone) {
            result.compareAndSet(null, Try(f(af.get, bf.get)))
          }

          override def cancel(mayInterruptIfRunning: Boolean): Boolean = {
            af.cancel(mayInterruptIfRunning)
            bf.cancel(mayInterruptIfRunning)
          }

          override def isCancelled: Boolean = af.isCancelled || bf.isCancelled

          override def isDone: Boolean = af.isDone && bf.isDone

          override def get(): C = {
            writeResultIfDone()
            Option(result.get()).map(_.get).getOrElse(throw new IllegalStateException("Incomplete"))
          }

          override def get(timeout: Long, unit: TimeUnit): C = {
            writeResultIfDone()
            if (isDone) get()
            else {
              val timeoutInMillis = unit.toMillis(timeout)
              val start = System.currentTimeMillis()
              af.get(timeout, unit)
              val end = System.currentTimeMillis()
              if (end < start + timeoutInMillis) {
                bf.get(timeoutInMillis - (end - start), TimeUnit.MILLISECONDS)
                writeResultIfDone()
                get()
              } else {
                throw new TimeoutException("Timed out")
              }
            }
          }
        }
      }

      def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

      def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

      def sequence[A](ps: List[Par[A]]): Par[List[A]] =
        map(ps.foldLeft(unit(List.empty[A])) { (acc, cur) => map2(cur, acc)(_ +: _) })(_.reverse)

      def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
        map(sequence(as.map(a => lazyUnit(a -> f(a)))))(_.flatMap {
          case (a, true) => Some(a)
          case _         => None
        })
    }
  }

}
