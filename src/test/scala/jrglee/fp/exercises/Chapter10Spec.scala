package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter07.Section5
import jrglee.fp.exercises.Chapter08.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.ForkJoinPool

class Chapter10Spec extends AnyFreeSpec with Matchers {

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

  "10.5" - {
    "should fold map a list" in {
      foldMap(List(1, 2, 3), stringMonoid)(_.toString) shouldBe "123"
    }
  }

  "10.6" - {
    "should foldLeft with foldMap" in {
      foldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
    }
    "should foldRight with foldMap" in {
      foldRight(List(1, 2, 3), 0)(_ + _) shouldBe 6
    }
  }

  "10.7" - {
    "should fold map binary splits" in {
      foldMapV(0 to 100, stringMonoid)(_.toString) shouldBe (0 to 100).mkString
    }

    "should handle empty" in {
      foldMapV(Array.empty[Int], stringMonoid)(_.toString) shouldBe ""
    }
  }

  "10.8" - {
    def run[A](p: Section5.Par[A]): A = Section5.Par.run(ForkJoinPool.commonPool())(p)

    "should fold map binary splits in parallel" in {
      run(Parallel.parFoldMap(0 to 100, stringMonoid)(_.toString)) shouldBe (0 to 100).mkString
    }

    "should handle empty" in {
      run(Parallel.parFoldMap(Array.empty[Int], stringMonoid)(_.toString)) shouldBe ""
    }
  }

  "10.9" - {
    "should work with small" in {
      isOrdered(Array(2, 1)) shouldBe false
    }

    "should validate ordered" in {
      isOrdered(0 to 100) shouldBe true
    }

    "should fail with unordered" in {
      isOrdered((0 to 75) ++ Seq(1) ++ (76 to 100)) shouldBe false
    }

    "should handle empty" in {
      isOrdered(Array.empty[Int]) shouldBe false
    }
  }
}
