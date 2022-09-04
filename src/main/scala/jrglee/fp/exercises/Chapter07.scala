package jrglee.fp.exercises

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, TimeUnit}
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

  object Section4 {
    trait Future[A] {
      def apply(k: A => Unit): Unit
    }

    type Par[A] = ExecutorService => Future[A]

    object Par {
      def run[A](es: ExecutorService)(p: Par[A]): Try[A] = {
        val ref = new AtomicReference[A]()
        val latch = new CountDownLatch(1)
        try {
          p(es) { a =>
            ref.set(a)
            latch.countDown()
          }
          latch.await()
          Try(ref.get)
        } catch {
          case e: Throwable =>
            latch.countDown()
            Try(throw e)
        }
      }
    }
  }

  object Section5 {
    trait Future[A] {
      def apply(k: A => Unit): Unit
    }

    type Par[A] = ExecutorService => Future[A]

    object Par {
      def unit[A](a: A): Par[A] = _ => (cb: A => Unit) => cb(a)

      def fork[A](a: => Par[A]): Par[A] = es => (cb: A => Unit) => eval(es)(a(es)(cb))

      def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] {
        override def call(): Unit = r
      })

      def map[A, B](pa: Par[A])(f: A => B): Par[B] = flatMap(pa)(a => unit(f(a)))

      def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = es =>
        (cb: B => Unit) => {
          val ra = new AtomicReference[A]()
          val latch = new CountDownLatch(1)
          pa(es) { a =>
            ra.set(a)
            latch.countDown()
          }
          latch.await()
          f(ra.get())(es)(cb)
        }

      def run[A](es: ExecutorService)(p: Par[A]): A = {
        val ref = new AtomicReference[A]()
        val latch = new CountDownLatch(1)
        p(es) { a =>
          ref.set(a)
          latch.countDown()
        }
        latch.await()
        ref.get()
      }

      def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = flatMap(n)(choices(_))

      def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = choiceN(map(cond)(if (_) 0 else 1))(List(t, f))

      def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = flatMap(key)(choices)

      def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = flatMap(pa)(choices)

      def join[A](ppa: Par[Par[A]]): Par[A] = es =>
        (cb: A => Unit) => {
          val ref = new AtomicReference[Par[A]]()
          val latch = new CountDownLatch(1)
          ppa(es) { a =>
            ref.set(a)
            latch.countDown()
          }
          latch.await()
          ref.get()(es)(cb)
        }

      def joinWithFlatMap[A](ppa: Par[Par[A]]): Par[A] = flatMap(ppa)(identity)

      def flatMapWithJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = join(map(pa)(f))

      def map2[A, B, C](pa: => Par[A], pb: => Par[B])(f: (A, B) => C): Par[C] =
        flatMap(pa)(a => flatMap(pb)(b => unit(f(a, b))))
    }
  }
}
