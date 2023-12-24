package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter05.{Cons, Empty, Stream}
import jrglee.fp.exercises.Chapter12.Monad
import jrglee.fp.exercises.Chapter13.IO

import java.io.FileWriter
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

      def onComplete(p: => Process[F, O]): Process[F, O] = this.onHalt {
        case End => p.asFinalizer
        case err => p.asFinalizer ++ Halt(err)
      }

      def asFinalizer: Process[F, O] = this match {
        case Emit(h, t) => Emit(h, t.asFinalizer)
        case Halt(e)    => Halt(e)
        case Await(req, recv) =>
          await(req) {
            case Left(Kill) => this.asFinalizer
            case x          => recv(x)
          }
      }

      def repeat: Process[F, O] = this ++ this.repeat

      def |>[O2](p2: Process1[O, O2]): Process[F, O2] = {
        p2 match {
          case Halt(e)    => this.kill onHalt { e2 => Halt(e) ++ Halt(e2) }
          case Emit(h, t) => Emit(h, this |> t)
          case Await(req, recv) =>
            this match {
              case Halt(err)          => Halt(err) |> recv(Left(err))
              case Emit(h, t)         => t |> Try(recv(Right(h)))
              case Await(req0, recv0) => await(req0)(recv0 andThen (_ |> p2))
            }
        }
      }

      def pipe[O2](p2: Process1[O, O2]): Process[F, O2] = this |> p2

      @tailrec
      final def kill[O2]: Process[F, O2] = this match {
        case Await(req, recv) =>
          recv(Left(Kill)).drain.onHalt {
            case Kill => Halt(End)
            case e    => Halt(e)
          }
        case Halt(e)    => Halt(e)
        case Emit(h, t) => t.kill
      }

      def drain[O2]: Process[F, O2] = this match {
        case Halt(e)          => Halt(e)
        case Emit(h, t)       => t.drain
        case Await(req, recv) => Await(req, recv andThen (_.drain))
      }

      def filter(f: O => Boolean): Process[F, O] = this |> Process.filter(f)

      def tee[O2, O3](p2: Process[F, O2])(t: Tee[O, O2, O3]): Process[F, O3] = t match {
        case Halt(e)    => this.kill.onComplete(p2.kill).onComplete(Halt(e))
        case Emit(h, t) => Emit(h, (this tee p2)(t))
        case Await(side, recv) =>
          side.get match {
            case Left(isO) =>
              this match {
                case Halt(e)            => p2.kill.onComplete(Halt(e))
                case Emit(o, ot)        => (ot tee p2)(Try(recv(Right(o))))
                case Await(reqL, recvL) => await(reqL)(recvL andThen (this2 => this2.tee(p2)(t)))
              }

            case Right(isO2) =>
              p2 match {
                case Halt(e)            => this.kill.onComplete(Halt(e))
                case Emit(o2, ot)       => (this tee ot)(Try(recv(Right(o2))))
                case Await(reqR, recvR) => await(reqR)(recvR andThen (p3 => this.tee(p3)(t)))
              }
          }
      }

      def zipWith[O2, O3](p2: Process[F, O2])(f: (O, O2) => O3): Process[F, O3] =
        (this tee p2)(Process.zipWith(f))

      def to[O2](sink: Sink[F, O]): Process[F, Unit] = join { this.zipWith(sink)((o, f) => f(o)) }
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
      def emit[F[_], O](head: O, tail: Process[F, O] = Halt[F, O](End)): Process[F, O] = Emit[F, O](head, tail)
      def halt[F[_], O](err: Throwable): Process[F, O] = Halt[F, O](err)

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

      def resource[R, O](acquire: IO[R])(use: R => Process[IO, O])(release: R => Process[IO, O]): Process[IO, O] =
        eval(acquire).flatMap(r => use(r).onComplete(release(r)))

      def eval[F[_], A](a: F[A]): Process[F, A] = await(a) {
        case Right(value) => emit[F, A](value)
        case Left(err)    => halt(err)
      }

      def eval_[F[_], A, B](a: F[A]): Process[F, B] = await(a) {
        case Right(value) => halt(End)
        case Left(err)    => halt(err)
      }

      case class Is[I]() {
        sealed trait f[X]
        val Get = new f[I] {}
      }

      def Get[I]: Is[I]#f[I] = Is[I]().Get

      type Process1[I, O] = Process[Is[I]#f, O]

      def await1[I, O](recv: I => Process1[I, O], fallback: Process1[I, O] = halt1[I, O]): Process1[I, O] =
        Await(
          Get[I],
          (e: Either[Throwable, I]) =>
            e match {
              case Left(End)    => fallback
              case Left(err)    => Halt(err)
              case Right(value) => Try(recv(value))
            }
        )

      def emit1[I, O](h: O, tl: Process1[I, O] = halt1[I, O]): Process1[I, O] = emit(h, tl)

      def halt1[I, O]: Process1[I, O] = Halt[Is[I]#f, O](End)

      def lift[I, O](f: I => O): Process1[I, O] = await1[I, O](i => emit(f(i))).repeat
      def filter[I](f: I => Boolean): Process1[I, I] = await1[I, I](i => if (f(i)) emit(i) else halt1).repeat

      case class T[I, I2]() {
        sealed trait f[X] { def get: Either[I => X, I2 => X] }
        val L = new f[I] { def get = Left(identity) }
        val R = new f[I2] { def get = Right(identity) }
      }

      def L[I, I2] = T[I, I2]().L
      def R[I, I2] = T[I, I2]().R

      type Tee[I, I2, O] = Process[T[I, I2]#f, O]

      def haltT[I, I2, O]: Tee[I, I2, O] = Halt[T[I, I2]#f, O](End)

      def awaitL[I, I2, O](recv: I => Tee[I, I2, O], fallback: => Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] =
        await[T[I, I2]#f, I, O](L) {
          case Left(End) => fallback
          case Left(err) => Halt(err)
          case Right(a)  => Try(recv(a))
        }

      def awaitR[I, I2, O](recv: I2 => Tee[I, I2, O], fallback: => Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] =
        await[T[I, I2]#f, I2, O](R) {
          case Left(End) => fallback
          case Left(err) => Halt(err)
          case Right(a)  => Try(recv(a))
        }

      def emitT[I, I2, O](h: O, tl: Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] = emit(h, tl)

      def zipWith[I, I2, O](f: (I, I2) => O): Tee[I, I2, O] =
        awaitL[I, I2, O](i => awaitR(i2 => emitT(f(i, i2)))).repeat

      def zip[I, I2]: Tee[I, I2, (I, I2)] = zipWith((_, _))

      type Sink[F[_], O] = Process[F, O => Process[F, Unit]]

      def fileW(file: String, append: Boolean = false): Sink[IO, String] = {
        resource[FileWriter, String => Process[IO, Unit]](IO(new FileWriter(file, append)))(w =>
          constant { (s: String) => eval[IO, Unit](IO(w.write(s))) }
        ) { w => eval_(IO(w.close)) }
      }

      def constant[A](a: A): Process[IO, A] = eval[IO, A](IO(a)).repeat

      def join[F[_], O](p: Process[F, Process[F, O]]): Process[F, O] = p.flatMap(identity)
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
        override def fail[A](t: Throwable): Task[A] = Task(IO(Left(t)))
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
