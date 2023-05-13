package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter12.{Applicative, Failure, Monad, Success, Validation}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

class Chapter12Spec extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks {

  val optionApplicative = new Applicative[Option] {
    override def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] = fab.flatMap(f => fa.map(f))
    override def unit[A](a: => A): Option[A] = Option(a)
  }

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

      forEvery(table) { (input, expected) => optionApplicative.sequence[Int](input) shouldEqual (expected) }
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
}
