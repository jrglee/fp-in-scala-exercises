package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter08.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class Chapter10Spec extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks {

  import Chapter10._

  "10.1" - {
    "intAddition" - {
      "should add numbers" in {
        intAddition.op(1, 2) shouldBe 3
        intAddition.op(1, intAddition.zero) shouldBe 1
      }
    }

    "intMultiplication" - {
      "should multiply numbers" in {
        intMultiplication.op(1, 2) shouldBe 2
        intMultiplication.op(2, intMultiplication.zero) shouldBe 2
      }
    }

    "booleanOr" - {
      "should logically or booleans" in {
        booleanOr.op(true, false) shouldBe true
        booleanOr.op(false, booleanOr.zero) shouldBe false
      }
    }

    "booleanAnd" - {
      "should logically and booleans" in {
        booleanAnd.op(true, false) shouldBe false
        booleanAnd.op(true, booleanAnd.zero) shouldBe true
      }
    }
  }

  "10.2" - {
    "should combine options" in {
      optionMonoid.op(None, None) shouldBe None
      optionMonoid.op(Some(1), None) shouldBe Some(1)
      optionMonoid.op(None, Some(1)) shouldBe Some(1)
      optionMonoid.op(Some(0), Some(1)) shouldBe Some(0)
      optionMonoid.op(Some(0), optionMonoid.zero) shouldBe Some(0)
    }
  }

  "10.3" - {
    "should make monoids in the category of endofunctors" in {
      endoMonoid.op((a: Int) => a * 2, (a: Int) => a + 2)(1) shouldBe 4
      endoMonoid.op((a: Int) => a * 2, endoMonoid.zero)(1) shouldBe 2
    }
  }

  "10.4" - {
    "should work with intAddition" in {
      Chapter08.run(monoidLaws(intAddition, Gen.choose(0, 1000)))
      succeed
    }
  }
}
