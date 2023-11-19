package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter07.Section5.Par
import jrglee.fp.exercises.Chapter12.Monad

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Chapter13 {

  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
    def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))
  }
  case class Return[F[_], A](a: A) extends Free[F, A]
  case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  type TailRec[A] = Free[Function0, A]
  type Async[A] = Free[Par, A]

  def freeMonad[F[_]]: Monad[({ type f[a] = Free[F, a] })#f] = new Monad[({ type f[a] = Free[F, a] })#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)
    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa.flatMap(f)
  }

  @tailrec
  def runTrampoline[A](a: Free[Function0, A]): A =
    a match {
      case Return(a)  => a
      case Suspend(r) => r()
      case FlatMap(x, f) =>
        x match {
          case Return(a)     => runTrampoline(f(a))
          case Suspend(r)    => runTrampoline(f(r()))
          case FlatMap(y, g) => runTrampoline(y flatMap (a => g(a) flatMap f))
        }
    }

  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] =
    step(a) match {
      case Return(a)              => F.unit(a)
      case Suspend(r)             => r
      case FlatMap(Suspend(r), f) => F.flatMap(r)(a => run(f(a)))
      case _                      => sys.error("Impossible; `step` eliminates these cases")
    }

  @tailrec
  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f)     => step(f(x))
    case _                         => a
  }

  sealed trait Console[A] {
    def toPar: Par[A]
    def toThunk: () => A
  }

  object Console {
    type ConsoleIO[A] = Free[Console, A]

    def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
    def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))
  }

  case object ReadLine extends Console[Option[String]] {
    override def toPar: Par[Option[String]] = Par.lazyUnit(run)
    override def toThunk: () => Option[String] = () => run

    def run: Option[String] =
      try Some(readLine())
      catch { case e: Exception => None }
  }

  case class PrintLine(line: String) extends Console[Unit] {
    override def toPar: Par[Unit] = Par.lazyUnit(println(line))
    override def toThunk: () => Unit = () => println(line)
  }

  trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A] }
  type ~>[F[_], G[_]] = Translate[F, G]

  val consoleToFunction0 = new (Console ~> Function0) { def apply[A](f: Console[A]): () => A = f.toThunk }
  val consoleToPar = new (Console ~> Par) { def apply[A](f: Console[A]): Par[A] = f.toPar }

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] = {
    step(free) match {
      case Return(a)              => G.unit(a)
      case Suspend(r)             => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _                      => sys.error("Impossible; `step` eliminates these cases")
    }
  }

  implicit val function0Monad = new Monad[Function0] {
    override def unit[A](a: => A): () => A = () => a
    override def flatMap[A, B](a: Function0[A])(f: A => Function0[B]): Function0[B] = () => f(a())()
  }

  implicit val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = Par.fork { Par.flatMap(a)(f) }
  }

  def runConsoleFunction0[A](a: Free[Console, A]): () => A =
    runFree[Console, Function0, A](a)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console, A]): Par[A] =
    runFree[Console, Par, A](a)(consoleToPar)

  def translate[F[_], G[_], A](f: Free[F, A])(fg: F ~> G): Free[G, A] = {
    type FreeG[B] = Free[G, B]
    val ffG = new (F ~> FreeG) {
      override def apply[B](fb: F[B]): FreeG[B] = Suspend(fg(fb))
    }

    runFree(f)(ffG)(freeMonad[G])
  }

  def runConsole[A](a: Free[Console, A]): A = runTrampoline(translate(a)(consoleToFunction0))
}
