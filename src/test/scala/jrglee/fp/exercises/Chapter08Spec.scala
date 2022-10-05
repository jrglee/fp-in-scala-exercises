package jrglee.fp.exercises

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter08Spec extends AnyFreeSpec with Matchers {
  import Chapter08._

  "section 1" - {
    import org.scalacheck.Gen
    import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._

    "8.1" - {
      "should define properties of a sum" in {
        forAll(Gen.listOf(Gen.choose(0, 100))) { ns =>
          ns.sum shouldBe ns.reverse.sum
          ns.filter(_ > 0).sum shouldBe ns.sum
        }
        forAll(Gen.choose(1, 100)) { v => List.fill(5)(v).sum shouldBe 5 * v }
      }
    }

    "8.2" - {
      "should define properties of a max" in {
        forAll(Gen.nonEmptyListOf(Gen.choose(0, 100))) { ns =>
          ns.max shouldBe ns.reverse.max
          ns.max shouldBe ns.sorted.last
          ns.forall(_ <= ns.max) shouldBe true
        }
      }
    }
  }

  "section 2" - {
    import Chapter06.{FixedValueRNG, IncrementalValueRNG, ListOfValueRNG, SimpleRNG}
    import org.scalatest.prop.TableDrivenPropertyChecks._

    "8.4" - {
      "should generate from a range" in {
        val table = Table(("start", "stop"), (0, 10), (-10, 10), (500, 1000))

        forEvery(table) { (start, stop) =>
          val v = Gen.choose(start, stop).sample.run(SimpleRNG(System.currentTimeMillis()))._1
          v should be >= start
          v should be <= stop
        }
      }
    }

    "8.5" - {
      "should generate a fixed value" in {
        val table = Table("input") ++ (0 to 100)

        forEvery(table) { input =>
          Gen.unit(input).sample.run(SimpleRNG(0))._1 shouldBe input
        }
      }

      "should generate booleans" in {
        val table = Table("input") ++ (0 to 10)

        forEvery(table) { input =>
          Gen.boolean.sample.run(FixedValueRNG(input))._1 shouldBe input % 2 == 0
        }
      }

      "should generate a list values" in {
        Gen.listOfN(5, Gen.choose(0, 10)).sample.run(ListOfValueRNG((0 to 10).toList))._1 shouldEqual (0 until 5).toList
      }
    }

    "8.6" - {
      "should flatMap" in {
        val table = Table("input") ++ (0 to 100)

        forEvery(table) { input =>
          val a = Gen.choose(0, Int.MaxValue).flatMap(v => Gen.unit(v % 2 == 0)).sample.run(FixedValueRNG(input))._1
          val b = Gen.boolean.sample.run(FixedValueRNG(input))._1

          a shouldBe b
        }
      }

      "should generate dynamic lists" in {
        Gen.choose(0, 100).listOfN(Gen.choose(0, 5)).sample.run(ListOfValueRNG(List(2, 2, 4)))._1 shouldEqual List(2, 4)
      }
    }

    "8.7" - {
      "should pull from two generators with equal likelihood" in {
        val values = Gen.listOfN(10, Gen.union(Gen.unit(1), Gen.unit(2))).sample.run(IncrementalValueRNG(0))._1
        val grouped = values.groupBy(identity)
        grouped(1) should have size 5
        grouped(2) should have size 5
      }
    }

    "8.8" - {
      "should pull from weighted generators" in {
        val values = Gen
          .listOfN(12, Gen.weighted((Gen.unit(1), 1), (Gen.unit(2), 2)))
          .sample
          .run(ListOfValueRNG(List.fill(100)(List(0, Int.MaxValue / 2, Int.MaxValue)).flatten))
          ._1
        val grouped = values.groupBy(identity)
        grouped(1) should have size 4
        grouped(2) should have size 8
      }
    }

    "8.9" - {
      "&&" - {
        "should succeed both" in {
          val prop = Chapter08.forAll(Gen.unit(1))(_ < 10) &&
            Chapter08.forAll(Gen.choose(0, 10))(_ < 20)
          prop.run(100, 10, FixedValueRNG(5)) shouldBe Passed
        }

        "should fail right" in {
          val prop = Chapter08.forAll(Gen.unit(1))(_ < 10) &&
            Chapter08.forAll(Gen.unit(20))(_ < 10)
          prop.run(100, 10, FixedValueRNG(5)) shouldBe Falsified("20", 0)
        }

        "should fail left" in {
          val prop = Chapter08.forAll(Gen.unit(10))(_ < 10) &&
            Chapter08.forAll(Gen.unit(1))(_ < 10)
          prop.run(100, 10, FixedValueRNG(5)) shouldBe Falsified("10", 0)
        }
      }

      "||" - {
        "should succeed both" in {
          val prop = Chapter08.forAll(Gen.unit(1))(_ < 10) ||
            Chapter08.forAll(Gen.choose(0, 10))(_ < 20)
          prop.run(100, 10, FixedValueRNG(5)) shouldBe Passed
        }

        "should fail right" in {
          val prop = Chapter08.forAll(Gen.unit(1))(_ < 10) ||
            Chapter08.forAll(Gen.unit(20))(_ < 10)
          prop.run(100, 10, FixedValueRNG(5)) shouldBe Passed
        }

        "should fail left" in {
          val prop = Chapter08.forAll(Gen.unit(10))(_ < 10) ||
            Chapter08.forAll(Gen.unit(1))(_ < 10)
          prop.run(100, 10, FixedValueRNG(5)) shouldBe Passed
        }

        "should fail both" in {
          val prop = Chapter08.forAll(Gen.unit(10))(_ < 10) ||
            Chapter08.forAll(Gen.unit(20))(_ < 10)
          prop.run(100, 10, FixedValueRNG(5)) shouldBe Falsified("20", 0)
        }
      }
    }

    "8.10" - {
      "should convert Gen to SGen" in {
        val gen = Gen.unit(10)
        gen.unsized.forSize(1) shouldBe gen
      }
    }

    "8.11" - {
      "should flatMap" in {
        val sgen = Gen.unit(10).unsized.flatMap(v => Gen.unit(v + 1).unsized)
        sgen.forSize(1).sample.run(FixedValueRNG(5))._1 shouldBe 11
      }
    }

    "8.12" - {
      "should delegate size to SGen" in {
        SGen.listOf(Gen.unit(5)).forSize(3).sample.run(FixedValueRNG(1))._1 shouldEqual List(5, 5, 5)
      }
    }

    "8.13" - {
      "should generate nonempty lists" in {
        SGen.listOf1(Gen.unit(5)).forSize(0).sample.run(FixedValueRNG(1))._1 shouldEqual List(5)
      }
    }

    "8.14" - {
      "should validate sorted arrays of small numbers" in {
        SGen.sortedProp.run(100, 100, Chapter06.SimpleRNG(System.currentTimeMillis())) shouldBe Passed
      }
    }
  }
}
