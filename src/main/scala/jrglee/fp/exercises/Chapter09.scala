package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter08.{Gen, Prop, SGen, forAll}

import scala.language.implicitConversions
import scala.util.matching.Regex

object Chapter09 {

  trait Parsers[Parser[+_]] { self =>

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
    implicit def string(s: String): Parser[String]
    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
    implicit def regex(r: Regex): Parser[String]

    def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) | succeed(List.empty)
    def map[A, B](a: Parser[A])(f: A => B): Parser[B] = a.flatMap(pa => succeed(f(pa)))
    def product[A, B](a: Parser[A], b: => Parser[B]): Parser[(A, B)] = a.flatMap(pa => b.map((pa, _)))
    def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
    def succeed[A](a: A): Parser[A]
    def slice[A](p: Parser[A]): Parser[String]
    def map2[A, B, C](a: Parser[A], b: => Parser[B])(f: (A, B) => C): Parser[C] =
      a.flatMap(pa => b.map(pb => f(pa, pb)))
    def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if (n == 0) succeed(List.empty)
      else map2(p, listOfN(n - 1, p))(_ :: _)
    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
    def label[A](msg: String)(p: Parser[A]): Parser[A]
    def scope[A](msg: String)(p: Parser[A]): Parser[A]
    def attempt[A](p: Parser[A]): Parser[A]
    def errorLocation(e: ParseError): Location
    def errorMessage(e: ParseError): String

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
      def many: Parser[List[A]] = self.many(p)
      def slice: Parser[String] = self.slice(p)
      def label(msg: String): Parser[A] = self.label(msg)(p)
      def scope(msg: String): Parser[A] = self.scope(msg)(p)
      def attempt: Parser[A] = self.attempt(p)
    }

    object Laws {
      def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = forAll(in)(s => run(p1)(s) == run(p2)(s))
      def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)
      def productLaw[A, B](p: Parser[A])(in: Gen[String]): Prop = equal(p ** p, p.map(a => (a, a)))(in)
      def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop = forAll(inputs ** Gen.string) { case (input, msg) =>
        run(label(msg)(p))(input) match {
          case Left(e) => errorMessage(e) == msg
          case _       => true
        }
      }
    }
  }

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1        => offset + 1
      case lineStart => offset - lineStart
    }

    lazy val offsetInput: Option[String] = scala.util.Try(input.substring(offset)).toOption

    def toError(msg: String): ParseError = ParseError(List((this, msg)))
  }

  case class ParseError(stack: List[(Location, String)])

  type Parser[+A] = Location => Result[A]

  sealed trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]

  object MyParsers extends Parsers[Parser] {
    override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = p(Location(input)) match {
      case Success(get, _) => Right(get)
      case Failure(get)    => Left(get)
    }

    override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ???

    override implicit def string(s: String): Parser[String] = loc =>
      if (loc.offsetInput.exists(_.startsWith(s))) Success(s, loc.offset + s.length)
      else Failure(loc.toError(s"Expected: $s"))

    override implicit def regex(r: Regex): Parser[String] = loc =>
      r.findPrefixOf(loc.input) match {
        case Some(matched) => Success(matched, matched.length)
        case None          => Failure(loc.toError(s"Did not match: ${r.regex}"))
      }

    override def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

    override def slice[A](p: Parser[A]): Parser[String] = loc =>
      p(loc) match {
        case Success(_, n) => Success(loc.input.slice(loc.offset, loc.offset + n), n)
        case f: Failure    => f
      }

    override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

    override def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

    override def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

    override def attempt[A](p: Parser[A]): Parser[A] = ???

    override def errorLocation(e: ParseError): Location = ???

    override def errorMessage(e: ParseError): String = ???
  }

  trait JSON
  object JSON {
    case object JNull extends JSON
    case class JNumber(get: Double) extends JSON
    case class JString(get: String) extends JSON
    case class JBool(get: Boolean) extends JSON
    case class JArray(get: IndexedSeq[JSON]) extends JSON
    case class JObject(get: Map[String, JSON]) extends JSON

    def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
      import P._

      val spaces = char(' ').many.slice
      ???
    }
  }

}
