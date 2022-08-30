package jrglee.fp.exercises

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class Chapter05Spec extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks {
  import Chapter05._

  "5.1" - {
    "convert to list" in {
      val table = Table(
        ("input", "expected"),
        (Stream.empty, List.empty),
        (Stream(1), List(1)),
        (Stream(1, 2), List(1, 2)),
        (Cons(() => 1, () => Cons(() => 2, () => Empty)), List(1, 2))
      )

      forEvery(table) { (input, expected) => input.toList shouldEqual expected }
    }
  }

  "5.1" - {
    "get first n elements" in {
      val table = Table(
        ("input", "n", "expected"),
        (Stream.empty, 5, Stream.empty),
        (Stream(1), 2, Stream(1)),
        (Stream(1, 2), 1, Stream(1)),
        (Stream(1, 2, 3, 4), 3, Stream(1, 2, 3))
      )

      forEvery(table) { (input, n, expected) => input.take(n).toList shouldEqual expected.toList }
    }

    "skip first n elements" in {
      val table = Table(
        ("input", "n", "expected"),
        (Stream.empty, 1, Stream.empty),
        (Stream(1), 1, Stream.empty),
        (Stream(1, 2), 1, Stream(2)),
        (Stream(1, 2, 3, 4), 2, Stream(3, 4))
      )

      forEvery(table) { (input, n, expected) => input.drop(n).toList shouldEqual expected.toList }
    }
  }

  "5.3" - {
    "get all starting elements matching predicate" in {
      val table = Table(
        ("input", "expected"),
        (Stream.empty, Stream.empty),
        (Stream(1), Stream(1)),
        (Stream(1, 2), Stream(1, 2)),
        (Stream(1, 2, 3), Stream(1, 2, 3)),
        (Stream(1, 20, 3), Stream(1))
      )

      forEvery(table) { (input, expected) => input.takeWhile(_ < 10).toList shouldEqual expected.toList }
    }
  }

  "5.4" - {
    "check that all elements match a predicate" in {
      val table = Table(
        ("input", "expected"),
        (Stream.empty, true),
        (Stream(1), false),
        (Stream(2), true),
        (Stream(1, 2, 3), false),
        (Stream(2, 4, 6), true)
      )

      forEvery(table) { (input, expected) => input.forAll(_ % 2 == 0) shouldBe expected }
    }
  }

  "5.5" - {
    "takeWhile with foldRight" in {
      val table = Table(
        ("input", "expected"),
        (Stream.empty, Stream.empty),
        (Stream(1), Stream(1)),
        (Stream(1, 2), Stream(1, 2)),
        (Stream(1, 2, 3), Stream(1, 2, 3)),
        (Stream(1, 20, 3), Stream(1))
      )

      forEvery(table) { (input, expected) => input.takeWhile2(_ < 10).toList shouldEqual expected.toList }
    }
  }
}
