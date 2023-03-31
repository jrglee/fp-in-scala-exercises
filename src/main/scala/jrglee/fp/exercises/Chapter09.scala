package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter08.{Gen, Prop, SGen, forAll}

import java.util.regex.Pattern
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
    def char(c: Char): Parser[Char] = string(c.toString) map (_.head)
    def succeed[A](a: A): Parser[A]
    def slice[A](p: Parser[A]): Parser[String]
    def map2[A, B, C](a: Parser[A], b: => Parser[B])(f: (A, B) => C): Parser[C] =
      a.flatMap(pa => b.map(pb => f(pa, pb)))
    def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if (n == 0) succeed(List.empty)
      else map2(p, listOfN(n - 1, p))(_ :: _)
    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
    def surround[A, B, C](begin: Parser[A], end: Parser[B])(p: Parser[C]): Parser[C] = for {
      _ <- begin.slice
      r <- p
      _ <- end.slice
    } yield r
    def trim[A](p: Parser[A]): Parser[A] = surround((char(' ') | char('\n')).many, (char(' ') | char('\n')).many)(p)
    def label[A](msg: String)(p: Parser[A]): Parser[A]
    def scope[A](msg: String)(p: Parser[A]): Parser[A]
    def attempt[A](p: Parser[A]): Parser[A]
    def errorLocation(e: ParseError): Location
    def errorMessage(e: ParseError): String

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
      def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
      def *>[B](p2: Parser[B]): Parser[B] = **(p2).map(_._2)
      def <*[B](p2: Parser[B]): Parser[A] = **(p2).map(_._1)
      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
      def many: Parser[List[A]] = self.many(p)
      def trim: Parser[A] = self.trim(p)
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

    def advanceBy(n: Int): Location = copy(offset = offset + n)
  }

  case class ParseError(stack: List[(Location, String)]) {
    def push(loc: Location, msg: String): ParseError = copy((loc, msg) :: stack)
    def label[A](s: String): ParseError = ParseError(latestLoc.map((_, s)).toList)
    def latestLoc: Option[Location] = latest.map(_._1)
    def latest: Option[(Location, String)] = stack.lastOption
  }

  type Parser[+A] = ParserState => Result[A]

  case class ParserState(loc: Location) {
    def advanceBy(n: Int): ParserState = ParserState(loc.advanceBy(n))
  }

  object ParserState {
    def apply(s: String): ParserState = ParserState(Location(s))
  }

  sealed trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, isCommitted) => Failure(f(e), isCommitted)
      case _                       => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, isCommitted = false)
      case _                => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _             => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n + m)
      case _             => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  object MyParsers extends Parsers[Parser] {
    override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = p(ParserState(Location(input))) match {
      case Success(get, _) => Right(get)
      case Failure(get, _) => Left(get)
    }

    override def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = { s =>
      p1(s) match {
        case Failure(_, false) => p2(s)
        case r                 => r
      }
    }

    override implicit def string(s: String): Parser[String] = ps =>
      ps.loc.offsetInput match {
        case Some(input) =>
          if (input.startsWith(s)) Success(s, s.length)
          else Failure(ps.loc.toError(s"Expected: $s"), isCommitted = false)
        case None =>
          Failure(ps.loc.toError(s"Offset higher than input, not possible to parse ${s}"), isCommitted = false)
      }

    override implicit def regex(r: Regex): Parser[String] = ps =>
      r.findPrefixOf(ps.loc.offsetInput.getOrElse("")) match {
        case Some(matched) => Success(matched, matched.length)
        case None          => Failure(ps.loc.toError(s"Did not match: ${r.regex}"), isCommitted = false)
      }

    override def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

    override def slice[A](p: Parser[A]): Parser[String] = ps =>
      p(ps) match {
        case Success(_, n) => Success(ps.loc.input.slice(ps.loc.offset, ps.loc.offset + n), n)
        case f: Failure    => f
      }

    override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = s =>
      p(s) match {
        case Success(a, n) => f(a)(s.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
        case e: Failure    => e
      }

    override def label[A](msg: String)(p: Parser[A]): Parser[A] = s => p(s).mapError(_.label(msg))

    override def scope[A](msg: String)(p: Parser[A]): Parser[A] = s => p(s).mapError(_.push(s.loc, msg))

    override def attempt[A](p: Parser[A]): Parser[A] = s => p(s).uncommit

    override def errorLocation(e: ParseError): Location = e.latest.map(_._1).getOrElse(Location("unknown", -1))

    override def errorMessage(e: ParseError): String = e.latest.map(_._2).getOrElse("Unknown")
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

      val str = (char('"') *> regex((".*?" + Pattern.quote("\"")).r).map(_.dropRight(1))).map(JString).trim
      val number = regex("[0-9]+(\\.[0-9]+)?".r).map(_.toDouble).attempt.map(JNumber).trim
      val bool = string("true").map(_ => JBool(true)).trim | string("false").map(_ => JBool(false)).trim
      val jnull = string("null").map(_ => JNull).trim
      val trailingComma = char(',').trim | succeed("")
      def value: Parser[JSON] = str | number | bool | array | obj | jnull
      def array: Parser[JArray] =
        surround(char('['), char(']'))((value <* trailingComma).many)
          .map(a => JArray(a.toVector))
          .trim
      def obj: Parser[JObject] =
        surround(char('{'), char('}'))(many((str.trim.map(_.get) <* char(':').trim) ** (value <* trailingComma)))
          .map(_.toMap)
          .map(JObject)
          .trim

      obj | array
    }
  }

}
