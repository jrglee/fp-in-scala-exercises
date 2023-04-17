package jrglee.fp.exercises

object Chapter11 {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))
    def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
      case Left(fa)  => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
  }

  val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](as: List[A])(f: A => B): List[B] = as.map(f)
  }

  trait Mon[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))
  }

  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))
    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))
    def sequence[A](lma: List[F[A]]): F[List[A]] = traverse(lma)(identity)
    def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = la.foldLeft(unit(List.empty[B])) { (acc, a) =>
      map2(acc, f(a))(_ :+ _)
    }
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))
    def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))
    def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
      ms.foldRight(unit(List.empty[A])) { (a, acc) =>
        flatMap(f(a)) {
          case true  => map2(unit(a), acc)(_ +: _)
          case false => acc
        }
      }

    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
    def flatMapWithCompose[A, B](fa: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => fa, f)(())
    def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)
    def composerWithJoinAndMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))
  }

  object Monad {
    import jrglee.fp.exercises.Chapter04.{Option => MyOption, Some => MySome}
    import jrglee.fp.exercises.Chapter05.Stream
    import jrglee.fp.exercises.Chapter03.{List => MyList}
    import jrglee.fp.exercises.Chapter06.State
    import jrglee.fp.exercises.Chapter07.Section5.Par
    import jrglee.fp.exercises.Chapter08.Gen

    val genMonad: Monad[Gen] = new Monad[Gen] {
      override def unit[A](a: => A): Gen[A] = Gen.unit(a)
      override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa.flatMap(f)
    }

    val parMonad: Monad[Par] = new Monad[Par] {
      override def unit[A](a: => A): Par[A] = Par.unit(a)
      override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(fa)(f)
    }

    val myOptionMonad: Monad[MyOption] = new Monad[MyOption] {
      override def unit[A](a: => A): MyOption[A] = MySome(a)
      override def flatMap[A, B](fa: MyOption[A])(f: A => MyOption[B]): MyOption[B] = fa.flatMap(f)
    }

    val optionMonad: Monad[Option] = new Monad[Option] {
      override def unit[A](a: => A): Option[A] = Option(a)
      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }

    val streamMonad: Monad[Stream] = new Monad[Stream] {
      override def unit[A](a: => A): Stream[A] = Stream(a)
      override def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] = fa.flatMap(f)
    }

    val myListMonad: Monad[MyList] = new Monad[MyList] {
      override def unit[A](a: => A): MyList[A] = MyList(a)
      override def flatMap[A, B](fa: MyList[A])(f: A => MyList[B]): MyList[B] = MyList.flatMap(fa)(f)
    }

    val listMonad: Monad[List] = new Monad[List] {
      override def unit[A](a: => A): List[A] = List(a)
      override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    }

    class StateMonad[S] {
      type StateS[A] = State[S, A]

      val monad: Monad[StateS] = new Monad[StateS] {
        override def unit[A](a: => A): StateS[A] = State.unit(a)
        override def flatMap[A, B](fa: StateS[A])(f: A => StateS[B]): StateS[B] = fa.flatMap(f)
      }
    }

    val idMonad: Monad[Id] = new Monad[Id] {
      override def unit[A](a: => A): Id[A] = Id(a)
      override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa.flatMap(f)
    }

    def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
      override def unit[A](a: => A): State[S, A] = State(s => (a, s))
      override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st.flatMap(f)
    }

    def getState[S]: State[S, S] = State(s => (s, s))
    def setState[S](s: => S): State[S, Unit] = State(_ => ((), s))
  }

  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  case class Reader[R, A](run: R => A)
  object Reader {
    def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
      override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
      override def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
        Reader(r => f(st.run(r)).run(r))
    }
  }
}
