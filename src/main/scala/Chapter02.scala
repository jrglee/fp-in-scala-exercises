package jrglee.fp.exercises

import scala.annotation.tailrec

object Chapter02 {

  object Ex1 {
    def fib(n: Int): Int = {
      @tailrec def loop(i: Int, a: Int, b: Int): Int = i match {
        case 0 => a
        case _ => loop(i - 1, b, a + b)
      }

      loop(n, 0, 1)
    }
  }

  object Ex2 {
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
      @tailrec def loop(a: A, ar: Array[A]): Boolean = {
        if (ar.isEmpty) true
        else if (ordered(a, ar.head)) loop(ar.head, ar.tail)
        else false
      }

      if (as.isEmpty) true
      else loop(as.head, as.tail)
    }
  }

  object Ex3 {
    def curry[A, B, C](f: (A, B) => C): A => (B => C) = { (a: A) => (b: B) =>
      f(a, b)
    }
  }

  object Ex4 {
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = { (a: A, b: B) =>
      f(a)(b)
    }
  }

  object Ex5 {
    def compose[A, B, C](f: B => C, g: A => B): A => C = { (a: A) => f(g(a)) }
  }
}
