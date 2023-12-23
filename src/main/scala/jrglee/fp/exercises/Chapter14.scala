package jrglee.fp.exercises
import scala.collection.mutable

object Chapter14 {

  sealed trait ST[S, A] { self =>
    protected def run(s: S): (A, S)

    def map[B](f: A => B): ST[S, B] = new ST[S, B] {
      override protected def run(s: S): (B, S) = {
        val (a, s1) = self.run(s)
        (f(a), s1)
      }
    }

    def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
      override protected def run(s: S): (B, S) = {
        val (a, s1) = self.run(s)
        f(a).run(s1)
      }
    }
  }

  object ST {
    def apply[S, A](a: => A): ST[S, A] = {
      lazy val memo = a
      new ST[S, A] {
        override protected def run(s: S): (A, S) = (memo, s)
      }
    }

    def runST[A](st: RunnableST[A]): A = st.apply[Unit].run(())._1
  }

  sealed trait STRef[S, A] {
    protected var cell: A
    def read: ST[S, A] = ST(cell)
    def write(a: A): ST[S, Unit] = new ST[S, Unit] {
      override protected def run(s: S): (Unit, S) = {
        cell = a
        ((), s)
      }
    }
  }

  object STRef {
    def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
      override protected var cell: A = a
    })
  }

  trait RunnableST[A] {
    def apply[S]: ST[S, A]
  }

  sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
    protected def value: Array[A]
    def size: ST[S, Int] = ST(value.size)

    def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
      override protected def run(s: S): (Unit, S) = {
        value(i) = a
        ((), s)
      }
    }

    def read(i: Int): ST[S, A] = ST(value(i))

    def freeze: ST[S, List[A]] = ST(value.toList)

    def fill(xs: Map[Int, A]): ST[S, Unit] = xs.foldLeft(ST[S, Unit](())) { case (acc, (i, v)) =>
      acc.flatMap(_ => write(i, v))
    }

    def swap(i: Int, j: Int): ST[S, Unit] = for {
      x <- read(i)
      y <- read(j)
      _ <- write(i, y)
      _ <- write(j, x)
    } yield ()

  }

  object STArray {
    def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] = ST(new STArray[S, A] {
      lazy val value: Array[A] = Array.fill(sz)(v)
    })

    def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] = ST(new STArray[S, A] {
      override lazy val value: Array[A] = xs.toArray
    })
  }

  def partition[S](arr: STArray[S, Int], n: Int, r: Int, pivot: Int): ST[S, Int] = for {
    pivotVal <- arr.read(pivot)
    _ <- arr.swap(pivot, r)
    j <- (n until r).foldLeft(ST[S, Int](n))((j, i) =>
      for {
        j2 <- j
        v <- arr.read(i)
        j3 <-
          if (v < pivotVal) arr.swap(i, j2).map(_ => j2 + 1)
          else ST[S, Int](j2)
      } yield j3
    )
    _ <- arr.swap(j, r)
  } yield j

  def qs[S](a: STArray[S, Int], n: Int, r: Int): ST[S, Unit] = {
    if (n < r)
      for {
        pi <- partition(a, n, r, n + (r - n) / 2)
        _ <- qs(a, n, pi - 1)
        _ <- qs(a, pi + 1, r)
      } yield ()
    else ST(())
  }

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs
    else
      ST.runST(new RunnableST[List[Int]] {
        override def apply[S]: ST[S, List[Int]] = for {
          arr <- STArray.fromList(xs)
          size <- arr.size
          _ <- qs(arr, 0, size - 1)
          sorted <- arr.freeze
        } yield sorted
      })

  sealed abstract class STMap[S, K, V] {
    protected def value: mutable.HashMap[K, V]

    def size: ST[S, Int] = ST(value.size)
    def get(k: K): ST[S, Option[V]] = ST(value.get(k))
    def +=(kv: (K, V)): ST[S, Unit] = ST(value += kv)
    def ++=(kvs: Iterable[(K, V)]): ST[S, Unit] = ST(value ++= kvs)
    def -=(k: K): ST[S, Unit] = ST(value -= k)
    def --=(ks: Iterable[K]): ST[S, Unit] = ST(value --= ks)

    def freeze: ST[S, Map[K, V]] = ST(Map.from(value))
  }

  object STMap {
    def apply[S, K, V](kvs: (K, V)*): ST[S, STMap[S, K, V]] = from(kvs)

    def empty[S, K, V]: ST[S, STMap[S, K, V]] = apply()

    def from[S, K, V](it: IterableOnce[(K, V)]): ST[S, STMap[S, K, V]] = ST(new STMap[S, K, V] {
      override protected val value: mutable.HashMap[K, V] = mutable.HashMap.from(it)
    })

    def fromMap[S, K, V](map: Map[K, V]): ST[S, STMap[S, K, V]] = from(map)
  }

}
