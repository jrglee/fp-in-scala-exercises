package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter08.{Gen, Prop, forAll}

import scala.language.implicitConversions
import scala.util.matching.Regex

object Chapter09 {

  trait Parsers[ParserError, Parser[+_]] { self =>

    def run[A](p: Parser[A])(input: String): Either[ParserError, A]

    def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
    implicit def string(s: String): Parser[String]
    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
    implicit def regex(r: Regex): Parser[String]

    def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) | succeed(List.empty)
    def map[A, B](a: Parser[A])(f: A => B): Parser[B] = a.flatMap(pa => succeed(f(pa)))
    def product[A, B](a: Parser[A], b: => Parser[B]): Parser[(A, B)] = a.flatMap(pa => b.map((pa, _)))
    def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
    def succeed[A](a: A): Parser[A] // = string("") map (_ => a)
    def slice[A](p: Parser[A]): Parser[String]
    def map2[A, B, C](a: Parser[A], b: => Parser[B])(f: (A, B) => C): Parser[C] =
      a.flatMap(pa => b.map(pb => f(pa, pb)))
    def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if (n == 0) succeed(List.empty)
      else map2(p, listOfN(n - 1, p))(_ :: _)
    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    val contextAware = for {
      n <- regex("([0-9])".r)
      r <- listOfN(n.toInt, char('a'))
    } yield r

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
      def many: Parser[List[A]] = self.many(p)
      def slice: Parser[String] = self.slice(p)
    }

    object Laws {
      def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = forAll(in)(s => run(p1)(s) == run(p2)(s))
      def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)
      def productLaw[A, B](p: Parser[A])(in: Gen[String]): Prop = equal(p ** p, p.map(a => (a, a)))(in)
    }
  }

  object JSONParser {
    def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
      import P._

      val spaces = char(' ').many.slice
      ???
    }

    trait JSON
    object JSON {
      case object JNull extends JSON
      case class JNumber(get: Double) extends JSON
    }
  }
}
