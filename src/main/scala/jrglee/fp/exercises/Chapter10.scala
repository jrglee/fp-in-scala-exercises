package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter03.Tree
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
  def dual[A](m: Monoid[A]) = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)
    override def zero: A = m.zero
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(gen)(v => m.op(v, m.zero) == v) &&
      forAll(gen ** gen ** gen) { case ((a, b), c) => m.op(m.op(a, b), c) == m.op(a, m.op(b, c)) }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = concatenate(as.map(f), m)
  def foldLeft[A, B](as: List[A], b: B)(f: (A, B) => B): B = foldMap(as, endoMonoid[B])(a => b2 => f(a, b2))(b)
  def foldRight[A, B](as: List[A], b: B)(f: (B, A) => B): B = foldMap(as, dual(endoMonoid[B]))(a => b2 => f(b2, a))(b)

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
          case (Some((min1, max1, lMatch)), Some((min2, max2, rMatch))) =>
            Some((Math.min(min1, min2), Math.max(max1, max2), lMatch && rMatch && max1 <= min2))
          case (None, x) => x
          case _         => None
        }

      override def zero: Option[(Int, Int, Boolean)] = None
    }

    if (v.isEmpty) false
    else foldMapV(v, m)(a => Some((a, a, true))).exists(_._3)
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(c1), Stub(c2))                                 => Stub(c1 + c2)
      case (Part(lStub, words, rStub), Stub(c))                 => Part(lStub, words, rStub + c)
      case (Stub(c), Part(lStub, words, rStub))                 => Part(c + lStub, words, rStub)
      case (Part(lStub1, words1, ""), Part("", words2, rStub2)) => Part(lStub1, words1 + words2, rStub2)
      case (Part(lStub1, words1, _), Part(_, words2, rStub2))   => Part(lStub1, words1 + words2 + 1, rStub2)
    }
    override def zero: WC = Part("", 0, "")
  }

  def countWords(str: String): Int =
    foldMapV(str.toIndexedSeq, wcMonoid) { c =>
      if (c.isWhitespace) wcMonoid.zero
      else Stub(c.toString)
    } match {
      case Stub(chars)               => Math.min(chars.length, 1)
      case Part(lStub, words, rStub) => Math.min(lStub.length, 1) + words + Math.min(rStub.length, 1)
    }

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
    def toList[A](fa: F[A]): List[A] = foldMap(fa)(a => List(a))(listMonoid)
  }

  val foldableList = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = concatenate(as.map(f))(mb)
  }

  val foldableIndexedSeq = new Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = concatenate(as.map(f))(mb)
  }

  val foldableStream = new Foldable[LazyList] {
    override def foldRight[A, B](as: LazyList[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: LazyList[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def foldMap[A, B](as: LazyList[A])(f: A => B)(mb: Monoid[B]): B = concatenate(as.map(f))(mb)
  }

  val foldableTree = new Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      foldMap(as)(a => (b: B) => f(a, b))(dual(endoMonoid[B]))(z)
    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      foldMap(as)(a => (b: B) => f(b, a))(endoMonoid[B])(z)
    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = Tree.fold(as)(f, mb.op)
  }

  val foldableOption = new Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      foldMap(as)(a => (b: B) => f(a, b))(endoMonoid[B])(z)
    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      foldMap(as)(a => (b: B) => f(b, a))(endoMonoid[B])(z)
    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as.fold(mb.zero)(f)
  }

  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (ma.op(a1._1, a2._1), mb.op(a1._2, a2._2))
    override def zero: (A, B) = (ma.zero, mb.zero)
  }

  def mapMergeMonoid[K, V](mv: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      override def zero: Map[K, V] = Map[K, V]()
      override def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, mv.op(a.getOrElse(k, mv.zero), b.getOrElse(k, mv.zero)))
        }
    }

  def functionMonoid[A, B](mb: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: A => B, a2: A => B): A => B = a => mb.op(a1(a), a2(a))
    override def zero: A => B = _ => mb.zero
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
}
