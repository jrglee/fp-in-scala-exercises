package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter08.{Gen, Prop, forAll}

object Chapter10 {

  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    override def zero: List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
    override def zero: A => A = identity
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(gen)(v => m.op(v, m.zero) == v) &&
      forAll(gen ** gen ** gen) { case ((a, b), c) => m.op(m.op(a, b), c) == m.op(a, m.op(b, c)) }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = concatenate(as.map(f), m)
  def foldLeft[A, B](as: List[A], b: B)(f: (A, B) => B): B = foldMap(as, endoMonoid[B])(a => b2 => f(a, b2))(b)
  def foldRight[A, B](as: List[A], b: B)(f: (B, A) => B): B = foldMap(
    as,
    new Monoid[B => B] {
      override def op(b1: B => B, b2: B => B): B => B = b2 andThen b1
      override def zero: B => B = identity
    }
  )(a => b2 => f(b2, a))(b)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.isEmpty) m.zero
    else if (v.length == 1) f(v.head)
    else {
      val (lhs, rhs) = v.splitAt(v.length / 2)
      m.op(foldMapV(lhs, m)(f), foldMapV(rhs, m)(f))
    }

  object Parallel {
    import jrglee.fp.exercises.Chapter07.Section5.Par
    def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
      override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
      override def zero: Par[A] = Par.unit(m.zero)
    }
    def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
      val parM = par(m)

      def inner(v2: IndexedSeq[A]): Par[B] = {
        if (v2.isEmpty) parM.zero
        else if (v2.length == 1) Par.map(Par.unit(v2.head))(f)
        else {
          val (lhs, rhs) = v2.splitAt(v2.length / 2)
          parM.op(inner(lhs), inner(rhs))
        }
      }

      inner(v)
    }
  }

  def isOrdered(v: IndexedSeq[Int]): Boolean = {
    val m = new Monoid[Option[(Int, Int, Boolean)]] {
      override def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
        (a1, a2) match {
          case (Some((min1, max1, true)), Some((min2, max2, true))) =>
            if (max1 <= min2) Some((min1, max2, true))
            else Some((Math.min(min1, min2), Math.max(max1, max2), false))
          case (Some((min1, max1, _)), Some((min2, max2, _))) =>
            Some((Math.min(min1, min2), Math.max(max1, max2), false))
          case (None, x) => x
          case _         => None
        }

      override def zero: Option[(Int, Int, Boolean)] = None
    }

    if (v.isEmpty) false
    else
      foldMapV(v, m)((a: Int) => Some((a, a, true))) match {
        case Some((_, _, r)) => r
        case _               => true
      }
  }
}
