package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter07.Section5.Par
import jrglee.fp.exercises.Chapter12.Monad

import scala.annotation.tailrec

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
}
