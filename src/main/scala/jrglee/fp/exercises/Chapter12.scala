package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter03.{Branch, Leaf, Tree}
import jrglee.fp.exercises.Chapter06.State
import jrglee.fp.exercises.Chapter10.{Foldable, Monoid}
import jrglee.fp.exercises.Chapter11.Functor

object Chapter12 {

  trait Applicative[F[_]] extends Functor[F] { self =>
    // primitive combinators
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f, a) => f(a))
    def unit[A](a: => A): F[A]

    // derived combinators
    def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)
    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
      apply(apply(map(fa)(f.curried))(fb))(fc)
    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)

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
    def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = ofa.foldLeft(unit(Map.empty[K, V])) {
      case (facc, (k, fv)) => map2(facc, fv)((acc, v) => acc + (k -> v))
    }
  }

  object Applicative {
    type Id[A] = A

    val idApplicative = new Applicative[Id] {
      override def apply[A, B](fab: Id[A => B])(fa: Id[A]): Id[B] = fab(fa)
      override def unit[A](a: => A): Id[A] = a
    }

    val optionApplicative = new Applicative[Option] {
      override def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] = fab.flatMap(f => fa.map(f))

      override def unit[A](a: => A): Option[A] = Option(a)
    }

    val listApplicative = new Applicative[List] {
      override def apply[A, B](fab: List[A => B])(fa: List[A]): List[B] = fab.flatMap(f => fa.map(f))

      override def unit[A](a: => A): List[A] = List(a)
    }

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
      override def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa.flatMap(f)
    }

    def stateMonad[S]: Monad[({ type f[x] = State[S, x] })#f] = new Monad[({ type f[x] = State[S, x] })#f] {
      override def unit[A](a: => A): State[S, A] = State.unit(a)
      override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa.flatMap(f)
    }

    val listMonad: Monad[List] = new Monad[List] {
      override def unit[A](a: => A): List[A] = List(a)
      override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    }

    val optionMonad: Monad[Option] = new Monad[Option] {
      override def unit[A](a: => A): Option[A] = Option(a)
      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }

    def composeM[F[_], G[_]](implicit F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({ type f[x] = F[G[x]] })#f] = {
      new Monad[({ type f[x] = F[G[x]] })#f] {
        override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
        override def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
          F.flatMap(fa)(ga => F.map(T.traverse(ga)(f))(G.join))
      }
    }
  }

  sealed trait Validation[+E, +A]
  case class Failure[E](head: E, tail: Vector[E] = Vector.empty) extends Validation[E, Nothing]
  case class Success[A](a: A) extends Validation[Nothing, A]

  trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
    def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)
    def map[A, B](fa: F[A])(f: A => B): F[B] = {
      implicit val G: Applicative[Applicative.Id] = Applicative.idApplicative
      traverse(fa)(v => G.unit(f(v)))
    }

    override def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = ???
    override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = mapAccum(as, z) { (a, s) =>
      val b2 = f(s, a)
      (b2, b2)
    }._2

    override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
      traverse[({ type f[x] = Const[B, x] })#f, A, Nothing](as)(f)(monoidApplicative(mb))

    def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
      traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monad.stateMonad)

    def zipWithIndex[A](fa: F[A]): F[(A, Int)] = mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

    override def toList[A](fa: F[A]): List[A] = mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

    def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
      traverseS(fa)((a: A) =>
        (for {
          s1 <- State.get[S]
          (b, s2) = f(a, s1)
          _ <- State.set(s2)
        } yield b)
      ).run(s)

    def reverse[A](fa: F[A]): F[A] = mapAccum(fa, toList(fa).reverse)((a, s) => (s.head, s.tail))._1

    def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      mapAccum(fa, toList(fb)) {
        case (_, Nil)     => sys.error("zip: Incompatible shapes.")
        case (a, b :: bs) => ((a, b), bs)
      }._1

    def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
      mapAccum(fa, toList(fb)) {
        case (a, Nil)     => ((a, None), Nil)
        case (a, b :: bs) => ((a, Some(b)), bs)
      }._1

    def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
      mapAccum(fb, toList(fa)) {
        case (b, Nil)     => ((None, b), Nil)
        case (b, a :: as) => ((Some(a), b), as)
      }._1

    def fuse[G[_], H[_], A, B](
      fa: F[A]
    )(f: A => G[B], g: A => H[B])(G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
      traverse[({ type f[x] = (G[x], H[x]) })#f, A, B](fa)(a => (f(a), g(a)))(G.product(H))

    def compose[G[_]](implicit G: Traverse[G]): Traverse[({ type f[x] = F[G[x]] })#f] = {
      val self = this
      new Traverse[({ type f[x] = F[G[x]] })#f] {
        override def traverse[H[_]: Applicative, A, B](fa: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
          self.traverse(fa)(a => G.traverse(a)(f))
      }
    }
  }

  object Traverse {
    val listTraverse: Traverse[List] = new Traverse[List] {
      override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
        fa.foldRight(G.unit(List.empty[B]))((v, tail) => G.map2(f(v), tail)(_ +: _))
    }

    val optionTraverse: Traverse[Option] = new Traverse[Option] {
      override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
        fa.fold(G.unit(None: Option[B]))(v => G.map(f(v))(Option(_)))
    }

    val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
      override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
        Tree
          .fold[A, G[Tree[B]]](fa)(v => G.map(f(v))(ga => Leaf(ga)), (gl, gr) => G.map2(gl, gr)((l, r) => Branch(l, r)))
    }
  }

  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[
    ({
      type f[x] = Const[M, x]
    })#f
  ] = new Applicative[({ type f[x] = Const[M, x] })#f] {
    override def apply[A, B](fab: Const[M, A => B])(fa: Const[M, A]): Const[M, B] = M.op(fab, fa)
    override def unit[A](a: => A): Const[M, A] = M.zero
  }

}
