package jrglee.fp.exercises

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class Chapter04Spec extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks {

  import Chapter04._

  "4.1" - {
    "with empty" - {
      val none: Option[Int] = None

      "should map to nothing" in {
        none.map(_ + 1) shouldBe None
      }

      "should flatMap to nothing" in {
        none.flatMap(v => Some(v)) shouldBe None
      }

      "should get default value" in {
        none.getOrElse(2) shouldBe 2
      }

      "should get alternative value" in {
        none.orElse(Some(2)) shouldBe Some(2)
      }

      "should filter nothing" in {
        none.filter(_ => true) shouldBe None
      }
    }

    "with value" - {
      "should map value" in {
        Some(1).map(_ + 1) shouldBe Some(2)
      }

      "should flatMap to nothing" in {
        Some(1).flatMap(_ => None) shouldBe None
      }

      "should get packed value" in {
        Some(1).getOrElse(2) shouldBe 1
      }

      "should ignore else" in {
        Some(1).orElse(None) shouldBe Some(1)
      }

      "should filter" in {
        def isEven(x: Int) = x % 2 == 0
        Some(1).filter(isEven) shouldBe None
        Some(2).filter(isEven) shouldBe Some(2)
      }
    }
  }

  "4.2" - {
    "should calculate variance" in {
      val table = Table(("input", "expected"), (Seq(), None), (Seq(1.0), Some(0.0)), (Seq(1.0, 2.0), Some(0.25)))
      forEvery(table) { (input, expected) => variance(input) shouldBe expected }
    }
  }

}
