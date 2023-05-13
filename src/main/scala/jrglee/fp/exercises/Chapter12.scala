package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter11.Functor

object Chapter12 {

  trait Applicative[F[_]] extends Functor[F] { self =>
    // primitive combinators
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]
    def unit[A](a: => A): F[A]

    // derived combinators
    def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)
    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
      apply(apply(map(fa)(f.curried))(fb))(fc)
    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)

    def applyFromUnitAndMap2[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f, a) => f(a))

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List.empty[B]))((a, fbs) => map2(f(a), fbs)(_ :: _))

    def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)
    def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)(_ -> _)
    def product[G[_]](G: Applicative[G]): Applicative[({ type f[x] = (F[x], G[x]) })#f] =
      new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
        override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
          (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
        override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      }
    def compose[G[_]](G: Applicative[G]): Applicative[({ type f[x] = F[G[x]] })#f] =
      new Applicative[({ type f[x] = F[G[x]] })#f] {
        override def apply[A, B](fab: F[G[A => B]])(fa: F[G[A]]): F[G[B]] =
          self.map2(fab, fa)((gab, ga) => G.apply(gab)(ga))
        override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
      }
  }

  object Applicative {
    val streamApplicative = new Applicative[LazyList] {
      override def apply[A, B](fab: LazyList[A => B])(fa: LazyList[A]): LazyList[B] =
        fab.zip(fa).map { case (f, a) => f(a) }

      override def unit[A](a: => A): LazyList[A] = LazyList.continually(a)
    }

    def validationApplicative[E]: Applicative[({ type f[x] = Validation[E, x] })#f] =
      new Applicative[({ type f[x] = Validation[E, x] })#f] {
        override def apply[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] = (fab, fa) match {
          case (Success(f), Success(a))         => unit(f(a))
          case (f: Failure[E], Success(_))      => f
          case (Success(_), f: Failure[E])      => f
          case (fa: Failure[E], fb: Failure[E]) => fa.copy(tail = fa.tail ++ Vector(fb.head) ++ fb.tail)
        }

        override def unit[A](a: => A): Validation[E, A] = Success(a)
      }
  }

  trait Monad[F[_]] extends Applicative[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
    def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)
    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

    override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)((a: A) => unit(f(a)))
    override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))
  }
  object Monad {
    def eitherMonad[E]: Monad[({ type f[x] = Either[E, x] })#f] = new Monad[({ type f[x] = Either[E, x] })#f] {
      override def apply[A, B](fab: Either[E, A => B])(fa: Either[E, A]): Either[E, B] = for {
        f <- fab
        a <- fa
      } yield f(a)

      override def unit[A](a: => A): Either[E, A] = Right(a)

      override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa.flatMap(f)
    }
  }

  sealed trait Validation[+E, +A]
  case class Failure[E](head: E, tail: Vector[E] = Vector.empty) extends Validation[E, Nothing]
  case class Success[A](a: A) extends Validation[Nothing, A]

}
