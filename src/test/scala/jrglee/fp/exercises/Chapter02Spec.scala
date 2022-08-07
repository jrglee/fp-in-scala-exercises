package jrglee.fp.exercises

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class Chapter02Spec
    extends AnyFreeSpec
    with Matchers
    with TableDrivenPropertyChecks {
  "2.1" - {
    "should give Fibonacci value for a given position" in {
      val table = Table(
        ("input", "expected"),
        (0, 0),
        (1, 1),
        (2, 1),
        (3, 2),
        (4, 3),
        (5, 5),
        (6, 8)
      )

      forEvery(table) { (input, expected) =>
        Chapter02.Ex1.fib(input) shouldBe expected
      }
    }
  }

  "2.2" - {
    "should tell if an Array is sorted" in {
      val table = Table(
        ("input", "expected"),
        (Array(0), true),
        (Array(0, 1), true),
        (Array(1, 0), false),
        (Array(0, 1, 3), true),
        (Array(1, 1, 1), true),
        (Array(3, 1, 2), false)
      )

      forEvery(table) { (input, expected) =>
        Chapter02.Ex2.isSorted[Int](input, _ <= _) shouldBe expected
      }
    }
  }

  "2.3" - {
    "should curry" in {
      val curried: Int => Int => Int =
        Chapter02.Ex3.curry((a: Int, b: Int) => a + b)
      curried(1)(2) shouldBe 3
    }
  }

  "2.4" - {
    "should uncurry" in {
      val uncurried: (Int, Int) => Int =
        Chapter02.Ex4.uncurry((a: Int) => (b: Int) => a + b)
      uncurried(1, 2) shouldBe 3
    }
  }

  "2.5" - {
    "should compose" in {
      val composed: String => Boolean =
        Chapter02.Ex5.compose[String, Int, Boolean](_ % 2 == 0, _.length)
      composed("hello") shouldBe false
    }
  }
}
