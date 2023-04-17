package jrglee.fp.exercises

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter11Spec extends AnyFreeSpec with Matchers {
  import Chapter11._

  "11.3" - {
    "should sequence" in {
      Monad.listMonad.sequence(List(List(1), List(2), List(3))) shouldEqual List(List(1, 2, 3))
    }

    "should traverse" in {
      Monad.listMonad.traverse(List(1, 2, 3))(a => List(a)) shouldEqual List(List(1, 2, 3))
    }
  }

  "11.6" - {
    "should filter" in {
      Monad.optionMonad.filterM(List(1, 2, 3, 4, 5))(a => Option(a % 2 == 0)) shouldEqual Option(List(2, 4))
    }

    "should handle empty" in {
      Monad.optionMonad.filterM(List.empty[Int])(_ => None) shouldBe Option(List.empty)
    }

    "should handle no match" in {
      Monad.optionMonad.filterM(List(1, 2, 3))(_ => None) shouldBe empty
    }
  }

  "11.8" - {
    "should flatMap" in {
      Monad.optionMonad.flatMapWithCompose(Some(1))(v => Some(v * 2)) shouldEqual Some(2)
    }
  }

}
