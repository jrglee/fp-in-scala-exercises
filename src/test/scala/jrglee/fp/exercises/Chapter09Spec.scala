package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter09.JSON.{JArray, JString}
import jrglee.fp.exercises.Chapter09.{Failure, JSON, Location, ParseError, ParserState, Success, MyParsers => P}
import org.scalatest.Inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter09Spec extends AnyFreeSpec with Matchers with Inside {

  "section 6" - {
    "9.13" - {
      "string()" - {
        "should parse valid" in {
          P.string("foo")(ParserState("foo")) shouldBe Success("foo", 3)
        }

        "should shift offset" in {
          P.string("oo")(ParserState(Location("foo", 1))) shouldBe Success("oo", 2)
        }

        "should fail parsing" in {
          val location = Location("bar")
          inside(P.string("foo")(ParserState(location))) { case Failure(ParseError((`location`, _) :: Nil), false) =>
            succeed
          }
        }
      }

      "regex()" - {
        "should parse valid" in {
          P.regex("[a-z]+".r)(ParserState("abc123")) shouldBe Success("abc", 3)
        }

        "should fail" in {
          val location = Location("123")
          inside(P.regex("[a-z]+".r)(ParserState(location))) {
            case Failure(ParseError((`location`, _) :: Nil), false) =>
              succeed
          }
        }
      }

      "succeed()" - {
        "should succeed with any thing" in {
          P.succeed("a")(ParserState("123")) shouldBe Success("a", 0)
        }
      }

      "slice()" - {
        "should return the portion by another parser" in {
          P.slice(P.string("abc"))(ParserState("abcd")) shouldBe Success("abc", 3)
        }

        "should shift offset" in {
          P.slice(P.string("bc"))(ParserState(Location("abc", 1))) shouldBe Success("bc", 2)
        }
      }
    }

    "9.14" - {
      "should fail with stack" in {
        val loc = Location("b")
        inside(P.scope("A")(P.label("B")(P.string("a")))(ParserState(loc))) { case Failure(ParseError(stack), false) =>
          stack shouldEqual List((loc, "A"), (loc, "B"))
        }
      }
    }

    "9.15" - {
      "should parse a quoted string" in {
        inside(JSON.jsonParser(P)(ParserState(""""Hello""""))) { case Success(json, charsConsumed) =>
          json shouldBe JString("Hello")
          charsConsumed shouldBe 7
        }
      }

      "should parse a list of strings" in {
        inside(JSON.jsonParser(P)(ParserState("""[ "Hello", "world" ]"""))) { case Success(json, _) =>
          json shouldBe JArray(Vector(JString("Hello"), JString("world")))
        }
      }

      // TODO rest of the parser
    }
  }
}
