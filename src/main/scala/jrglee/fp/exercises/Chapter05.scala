package jrglee.fp.exercises

object Chapter05 {

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty      => None
      case Cons(h, t) => Some(h())
    }

    def toList: List[A] = this match {
      case Empty      => List.empty
      case Cons(h, t) => h() +: t().toList
    }

    def take(n: Int): Stream[A] = this match {
      case Empty      => this
      case Cons(h, t) => if (n == 0) Empty else Stream.cons(h(), t().take(n - 1))
    }

    def drop(n: Int): Stream[A] = this match {
      case Empty      => this
      case Cons(_, t) => if (n == 0) this else t().drop(n - 1)
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Empty => this
      case Cons(h, t) =>
        val head = h()
        if (p(head)) Stream.cons(head, t().takeWhile(p)) else Empty
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

    def forAll(p: A => Boolean): Boolean = this match {
      case Empty      => true
      case Cons(h, t) => p(h()) && t().forAll(p)
    }

    def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A]) { (v, acc) =>
      if (p(v)) Stream.cons(v, acc) else Empty
    }

    def headOption2: Option[A] = foldRight(None: Option[A]) { (v, _) => Some(v) }

    def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B]) { (v, acc) =>
      Stream.cons(f(v), acc)
    }

    def filter(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A]) { (v, acc) =>
      if (p(v)) Stream.cons(v, acc) else acc
    }

    def append[B >: A](other: Stream[B]): Stream[B] = foldRight(other) { (v, acc) =>
      Stream.cons(v, acc)
    }

    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B]) { (v, acc) =>
      f(v).append(acc)
    }

    def map2[B](f: A => B): Stream[B] = unfold(this) {
      case Empty      => None
      case Cons(h, t) => Some(f(h()), t())
    }

    def take2(n: Int): Stream[A] = unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n - 1))
      case _                        => None
    }

    def takeWhile3(p: A => Boolean): Stream[A] = unfold(this) {
      case Cons(h, t) =>
        val v = h()
        if (p(v)) Some(v, t()) else None
      case _ => None
    }

    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _                            => None
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h, t), Empty)          => Some((Some(h()), None), (t(), Empty))
      case (Empty, Cons(h, t))          => Some((None, Some(h())), (Empty, t()))
      case _                            => None
    }

    def startsWith[B >: A](s: Stream[B]): Boolean = unfold(this, s) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((h1() == h2(), (t1(), t2())))
      case (_, Empty)                   => None
      case (Empty, _)                   => Some(false, (Empty, Empty))
      case _                            => None
    }.forAll(identity)

    def tails: Stream[Stream[A]] = unfold(Option(this)) {
      case Some(s @ Cons(_, t)) => Some((s, Some(t())))
      case Some(Empty)          => Some((Stream.empty[A], None))
      case None                 => None
    }

    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = unfold(Option(this)) {
      case Some(s @ Cons(_, t)) => Some((s.foldRight(z)(f), Some(t())))
      case Some(Empty)          => Some((z, None))
      case None                 => None
    }
  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))
  def fibs: Stream[Int] = {
    def f(a: Int, b: Int): Stream[Int] = Stream.cons(a, f(b, a + b))
    f(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((v, s)) => Stream.cons(v, unfold(s)(f))
    case None         => Empty
  }

  def constant2[A](a: A): Stream[A] = unfold(a)(v => Some(v, v))
  def from2(n: Int): Stream[Int] = unfold(n)(a => Some(a, a + 1))
  def fibs2: Stream[Int] = unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }
}
