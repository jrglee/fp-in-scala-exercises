package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter05.{Cons, Empty, Stream}

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
  }
}
