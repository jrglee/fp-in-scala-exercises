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
}
