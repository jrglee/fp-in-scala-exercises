package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter05.{Cons, Empty, Stream}
import jrglee.fp.exercises.Chapter12.Monad

object Chapter15 {

  sealed trait Process[I, O] {
    import Process._

    def apply(s: Stream[I]): Stream[O] = this match {
      case Halt() => Stream()
      case Await(recv) =>
        s match {
          case Cons(h, t) => recv(Option(h()))(t())
          case Empty      => recv(None)(Empty)
        }
      case Emit(head, tail) => Stream.cons(head, tail(s))
    }

    def repeat: Process[I, O] = {
      def go(p: Process[I, O]): Process[I, O] = p match {
        case Halt() => go(this)
        case Await(recv) =>
          Await {
            case None => recv(None)
            case i    => go(recv(i))
          }
        case Emit(head, tail) => Emit(head, go(tail))
      }
      go(this)
    }

    def |>[O2](p2: Process[O, O2]): Process[I, O2] = p2 match {
      case Halt()           => Halt()
      case Emit(head, tail) => Emit(head, this |> tail)
      case aw @ Await(recv) =>
        this match {
          case Halt()           => Halt() |> recv(None)
          case Emit(head, tail) => tail |> recv(Some(head))
          case Await(recv2)     => Await(i => recv2(i) |> aw)
        }
    }

    def map[O2](f: O => O2): Process[I, O2] = this |> lift(f)

    def ++(p: => Process[I, O]): Process[I, O] = this match {
      case Halt()           => p
      case Emit(head, tail) => Emit(head, tail ++ p)
      case Await(recv)      => Await(recv andThen (_ ++ p))
    }

    def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
      case Halt()           => Halt()
      case Emit(head, tail) => f(head) ++ tail.flatMap(f)
      case Await(recv)      => Await(recv andThen (_ flatMap f))
    }

    def zipWithIndex: Process[I, (O, Int)] = {
      def go(p: Process[I, O], n: Int): Process[I, (O, Int)] = p match {
        case Halt()           => Halt()
        case Emit(head, tail) => Emit((head, n), go(tail, n))
        case Await(recv)      => Await(i => go(recv(i), n + 1))
      }
      go(this, 0)
    }

    def zip[O2](p2: Process[I, O2]): Process[I, (O, O2)] = (this, p2) match {
      case (Halt(), _)                              => Halt()
      case (_, Halt())                              => Halt()
      case (Emit(lHead, lTail), Emit(rHead, rTail)) => Emit((lHead, rHead), lTail.zip(rTail))
      case (l, r)                                   => Await(i => l.feed(i).zip(r.feed(i)))
    }

    def feed(i: Option[I]): Process[I, O] = this match {
      case Halt()           => Halt()
      case Emit(head, tail) => Emit(head, tail.feed(i))
      case Await(recv)      => recv(i)
    }

  }

  object Process {
    case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]
    case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]
    case class Halt[I, O]() extends Process[I, O]

    def liftOne[I, O](f: I => O): Process[I, O] = Await {
      case Some(i) => Emit(f(i))
      case None    => Halt()
    }

    def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

    def filter[I](p: I => Boolean): Process[I, I] = Await[I, I] {
      case Some(i) if p(i) => Emit(i)
      case _               => Halt()
    }.repeat

    def sum: Process[Double, Double] = {
      def go(acc: Double): Process[Double, Double] = Await {
        case Some(d) => Emit(d + acc, go(d + acc))
        case None    => Halt()
      }
      go(0.0)
    }

    def take[I](n: Int): Process[I, I] =
      if (n <= 0) Halt()
      else
        Await[I, I] {
          case Some(i) => Emit(i, take(n - 1))
          case None    => Halt()
        }

    def drop[I](n: Int): Process[I, I] =
      if (n <= 0) lift(identity)
      else
        Await[I, I] {
          case Some(_) => drop(n - 1)
          case None    => Halt()
        }

    def takeWhile[I](f: I => Boolean): Process[I, I] = Await {
      case Some(i) if f(i) => Emit(i, takeWhile(f))
      case _               => Halt()
    }

    def dropWhile[I](f: I => Boolean): Process[I, I] = Await {
      case Some(i) =>
        if (f(i)) dropWhile(f)
        else Emit(i, lift(identity))
      case _ => Halt()
    }

    def count[I]: Process[I, Int] = {
      def go(n: Int): Process[I, Int] = Await {
        case Some(_) => Emit(n + 1, go(n + 1))
        case None    => Halt()
      }
      go(0)
    }

    def mean: Process[Double, Double] = {
      def go(sum: Double, count: Int): Process[Double, Double] = Await {
        case Some(i) =>
          val sum2 = sum + i
          val count2 = count + 1
          Emit(sum2 / count2, go(sum2, count2))
        case None => Halt()
      }
      go(0, 0)
    }

    def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] = Await {
      case Some(i) => f(i, z) match { case (o, s2) => Emit(o, loop(s2)(f)) }
      case None    => Halt()
    }

    def count2[I]: Process[I, Int] = loop(0) { case (_, s) => (s + 1, s + 1) }

    def mean2: Process[Double, Double] = loop((0d, 0)) { case (i, (sum, count)) =>
      val sum2 = sum + i
      val count2 = count + 1
      (sum2 / count2, (sum2, count2))
    }

    def monad[I]: Monad[({ type f[x] = Process[I, x] })#f] = new Monad[({ type f[x] = Process[I, x] })#f] {
      override def unit[O](o: => O): Process[I, O] = Emit(o)

      override def flatMap[O, O2](p: Process[I, O])(f: O => Process[I, O2]): Process[I, O2] = p flatMap f
    }

    def mean3: Process[Double, Double] = sum.zip(count).map { case (s, c) => s / c }

    def exists[I](f: I => Boolean): Process[I, Boolean] = Await {
      case Some(i) if f(i) => Emit(true, Halt())
      case Some(i)         => Emit(false, exists(f))
      case None            => Halt()
    }
  }
}
