package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter09.{Failure, Location, MyParsers, ParseError, Parser, Success}
import org.scalatest.Inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter09Spec extends AnyFreeSpec with Matchers with Inside {

  "section 6" - {
    "9.13" - {
      "string()" - {
        "should parse valid" in {
          MyParsers.string("foo")(Location("foo")) shouldBe Success("foo", 3)
        }

        "should shift offset" in {
          MyParsers.string("oo")(Location("foo", 1)) shouldBe Success("oo", 3)
        }

        "should fail parsing" in {
          val location = Location("bar")
          inside(MyParsers.string("foo")(location)) { case Failure(ParseError((`location`, _) :: Nil)) => succeed }
        }
      }

      "regex()" - {
        "should parse valid" in {
          MyParsers.regex("[a-z]+".r)(Location("abc123")) shouldBe Success("abc", 3)
        }

        "should fail" in {
          val location = Location("123")
          inside(MyParsers.regex("[a-z]+".r)(location)) { case Failure(ParseError((location, _) :: Nil)) => succeed }
        }
      }

      "succeed()" - {
        "should succeed with any thing" in {
          MyParsers.succeed("a")(Location("123")) shouldBe Success("a", 0)
        }
      }

      "slice()" - {
        "should return the portion by another parser" in {
          MyParsers.slice(MyParsers.string("abc"))(Location("abcd")) shouldBe Success("abc", 3)
        }

        "should shift offset" in {
          MyParsers.slice(MyParsers.string("bc"))(Location("abc", 1)) shouldBe Success("bc", 3)
        }
      }
    }
  }
}
