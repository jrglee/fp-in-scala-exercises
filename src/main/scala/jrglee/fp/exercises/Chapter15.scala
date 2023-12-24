package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter05.{Cons, Empty, Stream}
import jrglee.fp.exercises.Chapter12.Monad
import jrglee.fp.exercises.Chapter13.IO

import java.util.concurrent.ExecutorService
import scala.annotation.tailrec

object Chapter15 {

  object Simple {
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
        case Some(i) =>
          f(i, z) match {
            case (o, s2) => Emit(o, loop(s2)(f))
          }
        case None => Halt()
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
        case Some(_)         => Emit(false, exists(f))
        case None            => Halt()
      }
    }

    def processFile[A, B](f: java.io.File, p: Process[String, A], z: B)(g: (B, A) => B): IO[B] = IO {
      @tailrec
      def go(ss: Iterator[String], cur: Process[String, A], acc: B): B = cur match {
        case Process.Halt() => acc
        case Process.Await(recv) =>
          val next = if (ss.hasNext) recv(Some(ss.next)) else recv(None)
          go(ss, next, acc)
        case Process.Emit(h, t) => go(ss, t, g(acc, h))
      }

      val s = io.Source.fromFile(f)
      try go(s.getLines, p, z)
      finally s.close
    }

    def toCelsius(fahrenheit: Double): Double = (5.0 / 9.0) * (fahrenheit - 32.0)

    def fahrenheitToCelsius: Process[String, String] =
      (Process.filter[String](v => !v.startsWith("#")) |> Process.filter[String](_.nonEmpty))
        .flatMap[Double](v => v.toDoubleOption.fold(Process.Halt[String, Double]().repeat)(d => Process.Emit(d)))
        .map(toCelsius)
        .map(_.toString)
  }

  object Generic {

    trait Process[F[_], O] {
      import Process._

      def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match {
        case Halt(e)          => Try(f(e))
        case Emit(h, t)       => Emit(h, t.onHalt(f))
        case Await(req, recv) => Await(req, recv andThen (_.onHalt(f)))
      }

      def ++(p: => Process[F, O]): Process[F, O] = this.onHalt {
        case End => p
        case err => Halt(err)
      }

      def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] = this match {
        case Halt(err)        => Halt(err)
        case Emit(o, t)       => Try(f(o)) ++ t.flatMap(f)
        case Await(req, recv) => Await(req, recv andThen (_ flatMap f))
      }

      def runLog(implicit F: MonadCatch[F]): F[IndexedSeq[O]] = {
        def go(cur: Process[F, O], acc: IndexedSeq[O]): F[IndexedSeq[O]] = cur match {
          case Emit(h, t)       => go(t, acc :+ h)
          case Halt(End)        => F.unit(acc)
          case Halt(err)        => F.fail(err)
          case Await(req, recv) => F.flatMap(F.attempt(req))(e => go(Try(recv(e)), acc))
        }

        go(this, IndexedSeq())
      }
    }

    object Process {
      case class Await[F[_], A, O](req: F[A], recv: Either[Throwable, A] => Process[F, O]) extends Process[F, O]
      case class Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]
      case class Halt[F[_], O](err: Throwable) extends Process[F, O]

      case object End extends Exception
      case object Kill extends Exception

      def Try[F[_], O](p: => Process[F, O]): Process[F, O] =
        try p
        catch { case e: Throwable => Halt(e) }

      def await[F[_], A, O](req: F[A])(recv: Either[Throwable, A] => Process[F, O]): Process[F, O] = Await(req, recv)

      def runLog[O](src: Process[IO, O]): IO[IndexedSeq[O]] = IO {
        val E = java.util.concurrent.Executors.newFixedThreadPool(4)

        @tailrec
        def go(cur: Process[IO, O], acc: IndexedSeq[O]): IndexedSeq[O] = cur match {
          case Emit(h, t) => go(t, acc :+ h)
          case Halt(End)  => acc
          case Halt(err)  => throw err
          case Await(req, recv) =>
            val next =
              try recv(Right(IO.unsafePerformIO(req)(E)))
              catch { case err: Throwable => recv(Left(err)) }
            go(next, acc)
        }

        try go(src, IndexedSeq())
        finally E.shutdown()
      }
    }

    trait MonadCatch[F[_]] extends Monad[F] {
      def attempt[A](a: F[A]): F[Either[Throwable, A]]
      def fail[A](t: Throwable): F[A]
    }

    case class Task[A](get: IO[Either[Throwable, A]]) {
      def flatMap[B](f: A => Task[B]): Task[B] = Task(get.flatMap {
        case Left(e)  => IO(Left(e))
        case Right(a) => f(a).get
      })

      def run(implicit E: ExecutorService): A = IO.unsafePerformIO(get) match {
        case Left(e)  => throw e
        case Right(a) => a
      }

      def attemptRun(implicit E: ExecutorService): Either[Throwable, A] =
        try IO.unsafePerformIO(get)
        catch { case t: Throwable => Left(t) }
    }

    object Task {
      implicit val monadCatch: MonadCatch[Task] = new MonadCatch[Task] {
        override def attempt[A](a: Task[A]): Task[Either[Throwable, A]] = Task(a.get.map(v => Right(v)))
        override def fail[A](t: Throwable): Task[A] = ???
        override def unit[A](a: => A): Task[A] = Task.unit(a)
        override def flatMap[A, B](fa: Task[A])(f: A => Task[B]): Task[B] = fa.flatMap(f)
      }

      def unit[A](a: => A): Task[A] = Task(IO(Try(a)))

      def Try[A](a: => A): Either[Throwable, A] =
        try Right(a)
        catch { case e: Throwable => Left(e) }
    }
  }
}
