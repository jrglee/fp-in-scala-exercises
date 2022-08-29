package jrglee.fp.exercises

object Chapter04 {

  trait Option[+A] {
    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
    def getOrElse[B >: A](default: => B): B
    def orElse[B >: A](ob: => Option[B]): Option[B]
    def filter(F: A => Boolean): Option[A]
  }

  case object None extends Option[Nothing] {
    override def map[B](f: Nothing => B): Option[B] = None

    override def flatMap[B](f: Nothing => Option[B]): Option[B] = None

    override def getOrElse[B >: Nothing](default: => B): B = default

    override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob

    override def filter(F: Nothing => Boolean): Option[Nothing] = None
  }

  case class Some[A](value: A) extends Option[A] {
    override def map[B](f: A => B): Option[B] = Some(f(value))

    override def flatMap[B](f: A => Option[B]): Option[B] = f(value)

    override def getOrElse[B >: A](default: => B): B = value

    override def orElse[B >: A](ob: => Option[B]): Option[B] = this

    override def filter(f: A => Boolean): Option[A] = if (f(value)) this else None
  }

  def variance(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None
  else {
    val mean = xs.sum / xs.length
    Some(xs.map(x => math.pow(x - mean, 2)).sum / xs.length)
  }

  object Option {
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
      case (Some(a2), Some(b2)) => Some(f(a2, b2))
      case _                    => None
    }

    def sequence[A](a: List[Option[A]]): Option[List[A]] =
      a.foldLeft(Some(List.empty): Option[List[A]]) { (acc, v) =>
        map2(v, acc)(_ +: _)
      }.map(_.reverse)

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
      a.foldLeft(Some(List.empty): Option[List[B]]) { (acc, v) =>
        acc.flatMap(tail => f(v).map(_ +: tail))
      }.map(_.reverse)

    def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)
  }

  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
    def map2[EE >: E, B, C](b: => Either[EE, B])(f: (A, B) => C): Either[EE, C]
  }

  case class Right[A](value: A) extends Either[Nothing, A] {
    override def map[B](f: A => B): Either[Nothing, B] = Right(f(value))

    override def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = f(value)

    override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = this

    override def map2[EE >: Nothing, B, C](b: => Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      b.map(value2 => f(value, value2))
  }

  case class Left[E](error: E) extends Either[E, Nothing] {
    override def map[B](f: Nothing => B): Either[E, B] = this

    override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = this

    override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b

    override def map2[EE >: E, B, C](b: => Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this
  }

  object Either {
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      as.foldLeft(Right(List.empty): Either[E, List[B]]) { (acc, a) =>
        acc.map2(f(a))((tail, head) => head +: tail)
      }.map(_.reverse)
  }
}
