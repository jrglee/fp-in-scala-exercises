package jrglee.fp.exercises

object Chapter05 {

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Empty      => List.empty
      case Cons(h, t) => h() +: t().toList
    }

    def take(n: Int): Stream[A] = this match {
      case Empty      => this
      case Cons(h, t) => if (n == 0) Empty else Cons(h, () => t().take(n - 1))
    }

    def drop(n: Int): Stream[A] = this match {
      case Empty      => this
      case Cons(_, t) => if (n == 0) this else t().drop(n - 1)
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Empty => this
      case Cons(h, t) =>
        val head = h()
        if (p(head)) Cons(() => head, () => t().takeWhile(p)) else Empty
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
      if (p(v)) Cons(() => v, () => acc) else Empty
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
}
