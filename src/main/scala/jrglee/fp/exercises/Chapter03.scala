package jrglee.fp.exercises

import scala.annotation.tailrec

object Chapter03 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil         => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def tail[A](lst: List[A]): List[A] = lst match {
      case Nil           => Nil
      case Cons(_, tail) => tail
    }

    @tailrec def drop[A](lst: List[A], n: Int): List[A] = lst match {
      case Nil           => Nil
      case l if n <= 0   => l
      case Cons(_, tail) => drop(tail, n - 1)
    }

    def setHead[A](lst: List[A], a: A): List[A] = lst match {
      case Nil           => Nil
      case Cons(_, tail) => Cons(a, tail)
    }

    @tailrec def dropWhile[A](lst: List[A], f: A => Boolean): List[A] =
      lst match {
        case Cons(head, tail) if f(head) => dropWhile(tail, f)
        case l                           => l
      }

    def append[A](lst1: List[A], lst2: List[A]): List[A] = lst1 match {
      case Nil              => lst2
      case Cons(head, tail) => Cons(head, append(tail, lst2))
    }

    def init[A](lst: List[A]): List[A] = lst match {
      case Nil              => Nil
      case Cons(_, Nil)     => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }

    def foldRight[A, B](lst: List[A], b: B)(f: (A, B) => B): B = lst match {
      case Nil              => b
      case Cons(head, tail) => f(head, foldRight(tail, b)(f))
    }

    def length[A](lst: List[A]): Int = foldRight(lst, 0)((_, b) => b + 1)

    @tailrec def foldLeft[A, B](lst: List[A], b: B)(f: (B, A) => B): B =
      lst match {
        case Nil              => b
        case Cons(head, tail) => foldLeft(tail, f(b, head))(f)
      }

    def sumWithFoldLeft(lst: List[Int]): Int = foldLeft(lst, 0)(_ + _)

    def productWithFoldLeft(lst: List[Double]): Double =
      foldLeft(lst, 1.0)(_ * _)

    def reverseWithFold[A](lst: List[A]): List[A] =
      foldLeft(lst, Nil: List[A])((acc, v) => Cons(v, acc))

    @tailrec def foldLeft2[A, B](lst: List[A], b: B)(f: (B, A) => B): B =
      lst match {
        case Nil => b
        case Cons(head, tail) =>
          foldLeft2(tail, foldRight(List(head), b)((a, b) => f(b, a)))(f)
      }

    def foldRight2[A, B](lst: List[A], b: B)(f: (A, B) => B): B = lst match {
      case Nil => b
      case l =>
        val reverse = foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))
        foldLeft(reverse, b)((b, a) => f(a, b))
    }

    def append2[A](lst1: List[A], lst2: List[A]): List[A] =
      foldRight2(lst1, lst2)((a, b) => Cons(a, b))

    def flattenWithFold[A](lst: List[List[A]]): List[A] =
      foldRight2(lst, Nil: List[A])(append2)

    def addOne(lst: List[Int]): List[Int] =
      foldRight2(lst, Nil: List[Int])((v, t) => Cons(v + 1, t))

    def stringifyDoubles(lst: List[Double]): List[String] =
      foldRight2(lst, Nil: List[String])((v, t) => Cons(v.toString, t))

    def map[A, B](lst: List[A])(f: A => B): List[B] =
      foldRight2(lst, Nil: List[B])((a, b) => Cons(f(a), b))

    def filter[A](lst: List[A])(f: A => Boolean): List[A] =
      foldRight2(lst, Nil: List[A])((v, t) => if (f(v)) Cons(v, t) else t)

    def flatMap[A, B](lst: List[A])(f: A => List[B]): List[B] =
      foldRight2(lst, Nil: List[B])((v, t) => append(f(v), t))

    def filter2[A](lst: List[A])(f: A => Boolean): List[A] =
      flatMap(lst)(v => if (f(v)) List(v) else Nil)

    def zipInts(lst1: List[Int], lst2: List[Int]): List[Int] = {
      @tailrec def loop(a: List[Int], b: List[Int], acc: List[Int]): List[Int] = (a, b) match {
        case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(h1 + h2, acc))
        case _                            => acc
      }

      foldLeft(loop(lst1, lst2, Nil), Nil: List[Int])((b, a) => Cons(a, b))
    }

    def zipWith[A, B, C](lst1: List[A], lst2: List[B])(f: (A, B) => C): List[C] = {
      @tailrec def loop(left: List[A], right: List[B], acc: List[C]): List[C] =
        (left, right) match {
          case (Cons(lHead, lTail), Cons(rHead, rTail)) =>
            loop(lTail, rTail, Cons(f(lHead, rHead), acc))
          case _ => acc
        }

      foldLeft(loop(lst1, lst2, Nil), Nil: List[C])((b, a) => Cons(a, b))
    }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      @tailrec def scanMatch(lst: List[A], remain: List[A]): Boolean =
        (lst, remain) match {
          case (_, Nil) => true
          case (Nil, _) => false
          case (Cons(h1, t1), Cons(h2, t2)) =>
            if (h1 == h2) scanMatch(t1, t2) else false
        }

      @tailrec def loop(lst: List[A]): Boolean = {
        if (scanMatch(lst, sub)) true
        else
          lst match {
            case Nil           => false
            case Cons(_, tail) => loop(tail)
          }
      }

      loop(sup)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def size[A](tree: Tree[A]): Int = tree match {
      case Leaf(_)             => 1
      case Branch(left, right) => size(left) + size(right) + 1
    }

    def maximum[A](tree: Tree[A])(implicit ordering: Ordering[A]): A =
      tree match {
        case Leaf(value) => value
        case Branch(left, right) =>
          ordering.max(maximum(left), maximum(right))
      }

    def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(_)             => 1
      case Branch(left, right) => 1 + Math.max(depth(left), depth(right))
    }

    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(value)         => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

    def fold[A, B](tree: Tree[A])(map: A => B, acc: (B, B) => B): B =
      tree match {
        case Leaf(value) => map(value)
        case Branch(left, right) =>
          acc(fold(left)(map, acc), fold(right)(map, acc))
      }

    def size2[A](tree: Tree[A]): Int =
      fold[A, Int](tree)((_: A) => 1, (l: Int, r: Int) => l + r + 1)

    def maximum2[A](tree: Tree[A])(implicit ordering: Ordering[A]): A =
      fold(tree)(identity, (l: A, r: A) => ordering.max(l, r))

    def depth2[A](tree: Tree[A]): Int =
      fold(tree)((_: A) => 1, (l: Int, r: Int) => Math.max(l, r) + 1)

    def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      fold(tree)((a: A) => Leaf(f(a)), (l: Tree[B], r: Tree[B]) => Branch(l, r))
  }
}
