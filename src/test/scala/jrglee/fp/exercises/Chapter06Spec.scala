package jrglee.fp.exercises

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class Chapter06Spec extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks {

  import Chapter06._

  "6.1" - {
    "should generate non-negative random" in {
      val table = Table(
        ("input", "expected"),
        (0, 0),
        (10, 10),
        (Int.MaxValue, Int.MaxValue),
        (-1, Int.MaxValue),
        (-10, Int.MaxValue - 9),
        (Int.MinValue, 0)
      )

      forEvery(table) { (input, expected) => nonNegativeInt(FixedValueRNG(input))._1 shouldEqual expected }
    }
  }

  "6.2" - {
    "should generate a double from integer random" in {
      val table = Table("input", 0.5, 0.25, 0.75, 0.95, 1.0, 0.0, 0.1, 0.001)

      forEvery(table) { input => double(FixedValueRNG((input * Int.MaxValue).toInt))._1 shouldBe input +- 0.00001 }
    }
  }

  "6.3" - {
    "should generate a int-double pair" in {
      val table = Table("input") ++ (1 to 100)

      forEvery(table) { input =>
        val ((_, doubleVal), _) = intDouble(SimpleRNG(input))

        doubleVal should be >= 0.0
        doubleVal should be <= 1.0
      }
    }

    "should generate a double-int pair" in {
      val table = Table("input") ++ (1 to 100)

      forEvery(table) { input =>
        val ((doubleVal, _), _) = doubleInt(SimpleRNG(input))

        doubleVal should be >= 0.0
        doubleVal should be <= 1.0
      }
    }

    "should generate a double-double-double 3-tuple" in {
      val table = Table("input") ++ (1 to 100)

      forEvery(table) { input =>
        val ((d1, d2, d3), _) = double3(SimpleRNG(input))

        d1 should be >= 0.0
        d1 should be <= 1.0
        d2 should be >= 0.0
        d2 should be <= 1.0
        d3 should be >= 0.0
        d3 should be <= 1.0
      }
    }
  }

  "6.4" - {
    "should generate a list of random integers" in {
      val table = Table("input") ++ (1 to 100)

      forEvery(table) { input =>
        ints(100)(SimpleRNG(input))._1 should have size 100
      }
    }
  }

  "6.5" - {
    "should generate a double from integer random using map" in {
      val table = Table("input", 0.5, 0.25, 0.75, 0.95, 1.0, 0.0, 0.1, 0.001)

      forEvery(table) { input =>
        doubleWithMap(FixedValueRNG((input * Int.MaxValue).toInt))._1 shouldBe input +- 0.00001
      }
    }
  }

  "6.6" - {
    "should combine two random state data types" in {
      map2(unit(1), unit(2))(_ + _)(SimpleRNG(10))._1 shouldBe 3
    }
  }

  "6.7" - {
    "should generate a list of random integers with sequence" in {
      val table = Table("input") ++ (1 to 100)

      forEvery(table) { input =>
        intsWithSequence(100)(SimpleRNG(input))._1 should have size 100
      }
    }
  }

  "6.8" - {
    "should get a non negative less than random number using a flatMap" in {
      val table = Table(("input", "expected"), (0, 0), (1, 1), (2, 2), (3, 3), (4, 4), (5, 0), (10, 0), (-10, 3))

      forEvery(table) { (input, expected) => nonNegativeLessThan(5)(IncrementalValueRNG(input))._1 shouldBe expected }
    }
  }

  "6.9" - {
    "should map using flatMap" in {
      mapWithFlatMap(unit(1))(_ + 1)(SimpleRNG(0))._1 shouldBe 2
    }

    "should map2 using flatMap" in {
      map2WithFlatMap(unit(1), unit(2.1))((_, _))(SimpleRNG(0))._1 shouldBe (1, 2.1)
    }
  }

  "6.10" - {
    "unit with state" in {
      RandState.unit(2).run(SimpleRNG(0))._1 shouldBe 2
    }

    "map with state" in {
      val (r, s) = RandState.int.map(_ + 5).run(IncrementalValueRNG(0))
      r shouldBe 5
      s shouldBe IncrementalValueRNG(1)
    }

    "map2 with state" in {
      val (r, s) = State.map2(RandState.int, RandState.int)((_, _)).run(IncrementalValueRNG(0))
      r shouldBe (0, 1)
      s shouldBe IncrementalValueRNG(2)
    }

    "sequence with state" in {
      val (r, s) = State.sequence(List(RandState.int, RandState.int, RandState.int)).run(IncrementalValueRNG(0))
      r shouldEqual List(0, 1, 2)
      s shouldBe IncrementalValueRNG(3)
    }
  }
}
