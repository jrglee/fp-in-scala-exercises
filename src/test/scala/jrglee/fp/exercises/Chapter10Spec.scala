package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter03.{Branch, Leaf}
import jrglee.fp.exercises.Chapter07.Section5
import jrglee.fp.exercises.Chapter08.Gen
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

import java.util.concurrent.ForkJoinPool

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

  "10.10" - {
    "should combine text" in {
      val table = Table(
        ("lhs", "rhs", "expected"),
        (Stub("a"), Stub("b"), Stub("ab")),
        (Stub("a"), Part("b", 0, ""), Part("ab", 0, "")),
        (Part("", 0, "a"), Stub("b"), Part("", 0, "ab")),
        (Part("", 0, "a"), Part("b", 0, ""), Part("", 1, "")),
        (Part("a", 0, ""), Part("", 0, "b"), Part("a", 0, "b"))
      )

      forEvery(table) { (lhs, rhs, expected) => wcMonoid.op(lhs, rhs) shouldBe expected }
    }

    "should satisfy laws" in {
      val gen: Gen[WC] = Gen.weighted(
        (
          for {
            lStub <- Gen.weighted((Gen.unit(""), 0.2d), (Gen.stringN(2), 0.8d))
            words <- Gen.choose(0, 10)
            rStub <- Gen.weighted((Gen.unit(""), 0.2d), (Gen.stringN(2), 0.8d))
          } yield Part(lStub, words, rStub),
          0.8d
        ),
        (Gen.stringN(2).map(Stub), 0.2d)
      )
      Chapter08.run(monoidLaws(wcMonoid, gen))
      succeed
    }
  }

  "10.11" - {
    "should count words" in {
      val table = Table(
        ("input", "count"),
        ("", 0),
        ("a", 1),
        ("a ", 1),
        (" a ", 1),
        ("a b", 2),
        ("lorem ipsum dolor sit amet, ", 5)
      )

      forEvery(table) { (input, count) => countWords(input) shouldBe count }
    }
  }

  "10.13" - {
    val table = Table(
      ("input", "expected"),
      (Leaf(0), "0"),
      (Branch(Leaf(0), Leaf(1)), "01"),
      (Branch(Branch(Leaf(0), Leaf(1)), Leaf(2)), "012")
    )

    "should foldRight" in {
      forEvery(table) { (input, expected) =>
        foldableTree.foldRight(input)("") { (v, acc) => v.toString + acc } shouldBe expected
      }
    }

    "should foldLeft" in {
      forEvery(table) { (input, expected) =>
        foldableTree.foldLeft(input)("") { (acc, v) => acc + v } shouldBe expected
      }
    }

    "should foldMap" in {
      forEvery(table) { (input, expected) =>
        foldableTree.foldMap(input)(_.toString)(stringMonoid) shouldBe expected
      }
    }
  }

  "10.14" - {
    "should foldRight" in {
      foldableOption.foldRight(None: Option[Int])(1)(_ + _) shouldBe 1
      foldableOption.foldRight(Some(1))(1)(_ + _) shouldBe 2
    }

    "should foldLeft" in {
      foldableOption.foldLeft(None: Option[Int])(1)(_ + _) shouldBe 1
      foldableOption.foldLeft(Some(1))(1)(_ + _) shouldBe 2
    }

    "should foldMap" in {
      foldableOption.foldMap(None: Option[Int])(_.toString)(stringMonoid) shouldBe ""
      foldableOption.foldMap(Some(1))(_.toString)(stringMonoid) shouldBe "1"
    }
  }

  "10.15" - {
    "should convert to list" in {
      foldableOption.toList(None) shouldBe empty
      foldableOption.toList(Option(1)) shouldEqual List(1)
    }
  }
}
