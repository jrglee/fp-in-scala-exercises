package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter12.Applicative
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

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
}
