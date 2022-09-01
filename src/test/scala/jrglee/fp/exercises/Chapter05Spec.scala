package jrglee.fp.exercises

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.util.Random

class Chapter05Spec extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks {
  import Chapter05._

  "5.1" - {
    "should convert to list" in {
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
    "should get first n elements" in {
      val table = Table(
        ("input", "n", "expected"),
        (Stream.empty, 5, Stream.empty),
        (Stream(1), 2, Stream(1)),
        (Stream(1, 2), 1, Stream(1)),
        (Stream(1, 2, 3, 4), 3, Stream(1, 2, 3))
      )

      forEvery(table) { (input, n, expected) => input.take(n).toList shouldEqual expected.toList }
    }

    "should skip first n elements" in {
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
    "should get all starting elements matching predicate" in {
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
    "should check that all elements match a predicate" in {
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
    "should takeWhile with foldRight" in {
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

  "5.6" - {
    "should get headOption with foldRight" in {
      val table = Table(
        ("input", "expected"),
        (Stream.empty, None),
        (Stream(1), Some(1)),
        (Stream(1, 2), Some(1)),
        (Stream(1, 2, 3), Some(1))
      )

      forEvery(table) { (input, expected) => input.headOption2 shouldBe expected }
    }
  }

  "5.7" - {
    "should map with foldRight" in {
      val table = Table(
        ("input", "expected"),
        (Stream.empty, Stream.empty),
        (Stream(1), Stream(2)),
        (Stream(1, 2), Stream(2, 4)),
        (Stream(1, 2, 3), Stream(2, 4, 6))
      )

      forEvery(table) { (input, expected) => input.map(_ * 2).toList shouldEqual expected.toList }
    }

    "should filter with foldRight" in {
      val table = Table(
        ("input", "expected"),
        (Stream.empty, Stream.empty),
        (Stream(1), Stream.empty),
        (Stream(1, 2), Stream(2)),
        (Stream(1, 2, 3), Stream(2))
      )

      forEvery(table) { (input, expected) => input.filter(_ % 2 == 0).toList shouldEqual expected.toList }
    }

    "should append with foldRight" in {
      val table = Table(
        ("a", "b", "expected"),
        (Stream.empty, Stream.empty, Stream.empty),
        (Stream(1), Stream.empty, Stream(1)),
        (Stream(1, 2), Stream(3), Stream(1, 2, 3)),
        (Stream(1), Stream(2, 3), Stream(1, 2, 3))
      )

      forEvery(table) { (a, b, expected) => a.append(b).toList shouldEqual expected.toList }
    }

    "should flatMap with foldRight" in {
      val table = Table(
        ("input", "expected"),
        (Stream.empty, Stream.empty),
        (Stream(1), Stream(1, 2)),
        (Stream(1, 2), Stream(1, 2, 2, 4))
      )

      forEvery(table) { (input, expected) => input.flatMap(v => Stream(v, v * 2)).toList shouldEqual expected.toList }
    }
  }

  "5.8" - {
    "should create a constant stream" in {
      val table = Table("input") ++ (1 to 10)

      forEvery(table) { input => constant(input).take(5).toList shouldEqual List.fill(5)(input) }
    }
  }

  "5.9" - {
    "should create an incremental stream from initial value" in {
      val table = Table("input") ++ List.fill(10)(Random.nextInt())

      forEvery(table) { input => from(input).take(10).toList shouldEqual input.until(input + 10) }
    }
  }

  "5.10" - {
    "should create an infinite Fibonacci stream" in {
      fibs.take(10).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    }
  }

  "5.11" - {
    "should generate stream with unfold" in {
      unfold(1)(v => Some(v * 2, v + 1)).take(5).toList shouldEqual List(2, 4, 6, 8, 10)
    }

    "should interrupt stream when state is empty" in {
      unfold(1)(v => if (v <= 5) Some((v, v + 1)) else None).toList shouldEqual List(1, 2, 3, 4, 5)
    }
  }

  "5.12" - {
    "should create a constant stream" in {
      val table = Table("input") ++ (1 to 10)

      forEvery(table) { input => constant2(input).take(5).toList shouldEqual List.fill(5)(input) }
    }

    "should create an infinite Fibonacci stream" in {
      fibs2.take(10).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    }

    "should create an incremental stream from initial value" in {
      val table = Table("input") ++ List.fill(10)(Random.nextInt())

      forEvery(table) { input => from2(input).take(10).toList shouldEqual input.until(input + 10) }
    }
  }

  "5.13" - {
    "should map with unfold" in {
      val table = Table(
        ("input", "expected"),
        (Stream.empty, Stream.empty),
        (Stream(1), Stream(2)),
        (Stream(1, 2), Stream(2, 4)),
        (Stream(1, 2, 3), Stream(2, 4, 6))
      )

      forEvery(table) { (input, expected) => input.map2(_ * 2).toList shouldEqual expected.toList }
    }

    "should take with unfold" in {
      val table = Table(
        ("input", "n", "expected"),
        (Stream.empty, 5, Stream.empty),
        (Stream(1), 2, Stream(1)),
        (Stream(1, 2), 1, Stream(1)),
        (Stream(1, 2, 3, 4), 3, Stream(1, 2, 3))
      )

      forEvery(table) { (input, n, expected) => input.take2(n).toList shouldEqual expected.toList }
    }

    "should takeWhile with unfold" in {
      val table = Table(
        ("input", "expected"),
        (Stream.empty, Stream.empty),
        (Stream(1), Stream(1)),
        (Stream(1, 2), Stream(1, 2)),
        (Stream(1, 2, 3), Stream(1, 2, 3)),
        (Stream(1, 20, 3), Stream(1))
      )

      forEvery(table) { (input, expected) => input.takeWhile3(_ < 10).toList shouldEqual expected.toList }
    }

    "should zipWith with unfold" in {
      val table = Table(
        ("a", "b", "expected"),
        (Stream.empty, Stream.empty, Stream.empty),
        (Stream(1), Stream.empty, Stream.empty),
        (Stream.empty, Stream(1), Stream.empty),
        (Stream(1), Stream(1), Stream(2)),
        (Stream(1, 2), Stream(3, 4), Stream(4, 6))
      )

      forEvery(table) { (a, b, expected) => a.zipWith(b)(_ + _).toList shouldEqual (expected.toList) }
    }

    "should zipAll with unfold" in {
      val table = Table(
        ("a", "b", "expected"),
        (Stream.empty, Stream.empty, Stream.empty),
        (Stream(1), Stream.empty, Stream((Some(1), None))),
        (Stream.empty, Stream(1), Stream((None, Some(1)))),
        (Stream(1), Stream(1), Stream((Some(1), Some(1)))),
        (Stream(1, 2), Stream(3), Stream((Some(1), Some(3)), (Some(2), None)))
      )

      forEvery(table) { (a, b, expected) => a.zipAll(b).toList shouldEqual (expected.toList) }
    }
  }

  "5.14" - {
    "should check startWith with unfold" in {
      val table = Table(
        ("input", "start", "expected"),
        (constant(1), Stream.empty, true),
        (Stream.empty, Stream(1), false),
        (from2(2), Stream(2, 3, 4), true),
        (Stream(1, 2), Stream(1), true),
        (Stream(1), Stream(1, 2), false),
        (Stream(1), Stream(1), true)
      )

      forEvery(table) { (input, start, expected) => input.startsWith(start) shouldBe expected }
    }
  }

  "5.15" - {
    "should generate streams with tails" in {
      val table = Table(
        ("input", "expected"),
        (Stream.empty, Stream(Stream.empty)),
        (Stream(1), Stream(Stream(1), Stream.empty)),
        (Stream(1, 2), Stream(Stream(1, 2), Stream(2), Stream.empty)),
        (Stream(1, 2, 3), Stream(Stream(1, 2, 3), Stream(2, 3), Stream(3), Stream.empty))
      )

      forEvery(table) { (input, expected) =>
        input.tails.map(_.toList).toList shouldEqual expected.map(_.toList).toList
      }
    }
  }

  "5.16" - {
    "should scanRight like tails" in {
      val table = Table(
        ("input", "expected"),
        (Stream.empty, Stream(Stream.empty)),
        (Stream(1), Stream(Stream(1), Stream.empty)),
        (Stream(1, 2), Stream(Stream(1, 2), Stream(2), Stream.empty)),
        (Stream(1, 2, 3), Stream(Stream(1, 2, 3), Stream(2, 3), Stream(3), Stream.empty))
      )

      forEvery(table) { (input, expected) =>
        input.scanRight(Empty: Stream[Int])((v, acc) => Stream.cons(v, acc)).map(_.toList).toList shouldEqual expected
          .map(_.toList)
          .toList
      }
    }

    "should work with the example from the book" in {
      Stream(1, 2, 3).scanRight(0)(_ + _).toList shouldEqual List(6, 5, 3, 0)
    }
  }
}
