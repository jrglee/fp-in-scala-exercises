package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter03.{Branch, Leaf}
import jrglee.fp.exercises.Chapter08.Gen
import jrglee.fp.exercises.Chapter12._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class Chapter12Spec extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks {

  import Applicative.{listApplicative, optionApplicative}

  "12.1" - {

    "should sequence" in {
      val table = Table[List[Option[Int]], Option[List[Int]]](
        ("input", "expected"),
        (List.empty, Option(List.empty)),
        (List(None), None),
        (List(Option(1)), Option(List(1))),
        (List(Option(1), Option(2)), Option(List(1, 2))),
        (List(Option(1), None), None)
      )

      forEvery(table) { (input, expected) => optionApplicative.sequence[Int](input) shouldEqual expected }
    }

    "should replicateM" in {
      val table = Table[Int, Option[Int], Option[List[Int]]](
        ("n", "input", "expected"),
        (2, Option(1), Option(List(1, 1))),
        (0, Option(1), Option(List.empty)),
        (2, None, None)
      )

      forEvery(table) { (n, input, expected) => optionApplicative.replicateM(n, input) shouldEqual expected }
    }

    "should product" in {
      val table = Table(
        ("lhs", "rhs", "expected"),
        (Option(1), Option(2), Option(1 -> 2)),
        (Option(1), None, None),
        (None, Option(2), None)
      )

      forEvery(table) { (lhs, rhs, expected) => optionApplicative.product(lhs, rhs) shouldEqual expected }
    }
  }

  "12.2" - {
    "should work like normal apply" in {
      val table = Table[Option[Int => Int], Option[Int]](
        ("f", "a"),
        (None, None),
        (None, Option(2)),
        (Option((_: Int) * 2), None),
        (Option((_: Int) * 2), Option(2))
      )

      forEvery(table) { (f, a) =>
        optionApplicative.apply(f)(a) shouldEqual optionApplicative.applyFromUnitAndMap2(f)(a)
      }
    }
  }

  "12.3" - {
    "should map3" in {
      optionApplicative.map3(Option(1), Option(2), Option(3))(_ + _ + _) shouldBe Option(6)
    }

    "should map4" in {
      optionApplicative.map4(Option(1), Option(2), Option(3), Option(4))(_ + _ + _ + _) shouldBe Option(10)
    }
  }

  "12.5" - {
    "should map Either" in {
      Monad.eitherMonad[String].map(Right(1))(_ * 2) shouldBe Right(2)
      Monad.eitherMonad[String].map(Left("foo"): Either[String, Int])(_ * 2) shouldBe Left("foo")
    }
  }

  "12.6" - {
    "should combine validation results" in {
      val table = Table[Validation[String, Int], Validation[String, Int], Validation[String, Int]](
        ("lhs", "rhs", "expected"),
        (Failure("Error"), Success(1), Failure("Error")),
        (Success(1), Success(2), Success(3)),
        (Failure("A"), Failure("B"), Failure("A", Vector("B")))
      )

      forEvery(table) { (lhs, rhs, expected) =>
        Applicative.validationApplicative[String].map2(lhs, rhs)(_ + _) shouldBe expected
      }
    }
  }

  "12.8" - {
    "should combine like monoids" in {
      optionApplicative
        .product(Applicative.validationApplicative[String])
        .map(Some(1) -> Success(2))(_ + 1) shouldBe (Some(2), Success(3))
    }
  }

  "12.9" - {
    "should compose" in {
      optionApplicative
        .compose(Applicative.validationApplicative[String])
        .map(Some(Success(2)))(_ + 1) shouldBe Some(Success(3))
    }
  }

  "12.12" - {
    "should sequence a map" in {
      val table = Table(
        ("input", "expected"),
        (Map(1 -> Some(1), 2 -> Some(2)), Some(Map(1 -> 1, 2 -> 2))),
        (Map(1 -> Some(1), 2 -> None), None)
      )

      forEvery(table) { (input, expected) => optionApplicative.sequenceMap(input) shouldEqual expected }
    }
  }

  "12.13" - {
    implicit val optionApp: Applicative[Option] = optionApplicative
    implicit val listApp: Applicative[List] = listApplicative

    "list" - {
      "should traverse" in {
        Traverse.listTraverse.sequence(List(Option(1), Option(2))) shouldEqual Option(List(1, 2))
        Traverse.listTraverse.sequence(List(Option(1), None)) shouldEqual None
      }
    }

    "option" - {
      "should traverse" in {
        Traverse.optionTraverse.sequence(Option(List(1))) shouldEqual List(Option(1))
        Traverse.optionTraverse.sequence(Option(List.empty[Int])) shouldEqual List.empty[Int]
      }
    }

    "tree" - {
      "should traverse" in {
        Traverse.treeTraverse.sequence(Branch(Leaf(Option(1)), Leaf(Option(2)))) shouldEqual Option(
          Branch(Leaf(1), Leaf(2))
        )
        Traverse.treeTraverse.sequence(Branch(Leaf(Option(1)), Leaf(None))) shouldEqual None
      }
    }
  }

  "12.14" - {
    "traverse" - {
      "should map list" in {
        Traverse.listTraverse.map(List(1, 2, 3))(_ * 2) shouldEqual List(2, 4, 6)
        Traverse.listTraverse.map(List.empty[Int])(_ * 2) shouldEqual List.empty[Int]
      }

      "should map option" in {
        Traverse.optionTraverse.map(Option(1))(_ * 2) shouldEqual Option(2)
        Traverse.optionTraverse.map(None: Option[Int])(_ * 2) shouldEqual None
      }

      "should map tree" in {
        Traverse.treeTraverse.map(Branch(Leaf(1), Leaf(2)))(_ * 2) shouldEqual Branch(Leaf(2), Leaf(4))
      }
    }
  }

  "12.15" - {
    "should foldMap with traverse" in {
      Traverse.listTraverse.foldMap(List(1, 2, 3))(_ * 2)(Chapter10.intAddition) shouldBe 12
    }
  }

  "12.16" - {
    "should satisfy laws" in {
      import Traverse.listTraverse._
      val lstGen = Gen.choose(0, 10).flatMap(n => Gen.listOfN(n, Gen.boolean))
      val prop = Chapter08.forAll(lstGen ** lstGen) { case (x, y) =>
        toList(reverse(x)) ++ toList(reverse(y)) == reverse(toList(y) ++ toList(x))
      }

      Chapter08.run(prop)
    }
  }

  "12.17" - {
    "should fold a list" in {
      val l = List(1, 2, 3)

      Traverse.listTraverse.foldLeft(l)(0)(_ + _) shouldBe l.sum
    }
  }
}
