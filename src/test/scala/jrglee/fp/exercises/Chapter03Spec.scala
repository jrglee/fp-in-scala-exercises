package jrglee.fp.exercises

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class Chapter03Spec
    extends AnyFreeSpec
    with Matchers
    with TableDrivenPropertyChecks {

  import Chapter03.Ref._

  "3.1" - {
    "should be 3" in {
      val x = List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _)))          => x
        case Nil                                   => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t)                            => h + List.sum(t)
        case _                                     => 101
      }

      x shouldBe 3
    }
  }

  "3.2" - {
    "should get tail of a list" in {
      val table = Table(
        ("input", "expected"),
        (Nil, Nil),
        (List(1), Nil),
        (List(1, 2, 3), List(2, 3))
      )

      forEvery(table) { (input, expected) =>
        List.tail(input) shouldBe expected
      }
    }
  }

  "3.3" - {
    "should set a different head" in {
      val table = Table(
        ("initial", "newHead", "expected"),
        (Nil, 2, Nil),
        (List(1), 2, List(2)),
        (List(1, 2, 3), 2, List(2, 2, 3))
      )

      forEvery(table) { (initial, newHead, expected) =>
        List.setHead(initial, newHead) shouldBe expected
      }
    }
  }

  "3.4" - {
    "should drop from the beginning of the list" in {
      val table = Table(
        ("initial", "n", "expected"),
        (Nil, 2, Nil),
        (List(1), 2, Nil),
        (List(1), -1, List(1)),
        (List(1, 2), 1, List(2)),
        (List(1, 2), 2, Nil)
      )

      forEvery(table) { (initial, n, expected) =>
        List.drop(initial, n) shouldBe expected
      }
    }
  }

  "3.5" - {
    "should drop following predicate" in {
      val table = Table(
        ("input", "expected"),
        (Nil, Nil),
        (List(1), Nil),
        (List(10), List(10)),
        (List(1, 2, 3, 4, 5), List(3, 4, 5))
      )

      forEvery(table) { (input, expected) =>
        List.dropWhile(input, (n: Int) => n < 3) shouldBe expected
      }
    }
  }

  "3.6" - {
    "should get all but the last item of a list" in {
      val table = Table(
        ("input", "expected"),
        (Nil, Nil),
        (List(1), Nil),
        (List(1, 2), List(1)),
        (List(1, 2, 3, 4, 5), List(1, 2, 3, 4))
      )

      forEvery(table) { (input, expected) =>
        List.init(input) shouldBe expected
      }
    }
  }

  "3.8" - {
    "should concat with foldRight" in {
      val table = Table(
        ("left", "right", "expected"),
        (List(1, 2, 3), Nil: List[Int], List(1, 2, 3))
      )

      forEvery(table) {
        (left: List[Int], right: List[Int], expected: List[Int]) =>
          List.foldRight(left, right)(Cons(_, _)) shouldBe expected
      }
    }
  }

  "3.9" - {
    "should compute the length with foldRight" in {
      val table =
        Table(("input", "length"), (Nil, 0), (List(1), 1), (List(1, 2), 2))

      forEvery(table) { (input, length) =>
        List.length(input) shouldBe length
      }
    }
  }

  "3.10" - {
    "should fold from the left" in {
      val table =
        Table(("input", "sum"), (Nil, 0), (List(1), 1), (List(1, 2, 3), 6))

      forEvery(table) { (input, sum) =>
        List.foldLeft(input, 0)(_ + _) shouldBe sum
      }
    }
  }

  "3.11" - {
    "should sum with foldLeft" in {
      val table =
        Table(("input", "sum"), (Nil, 0), (List(1), 1), (List(1, 2, 3), 6))

      forEvery(table) { (input, sum) =>
        List.sumWithFoldLeft(input) shouldBe sum
      }
    }

    "should multiply with foldLeft" in {
      val table = Table(
        ("input", "product"),
        (Nil, 1.0),
        (List(1.0), 1.0),
        (List(1.0, 2.0, 3.0), 6.0)
      )

      forEvery(table) { (input, product) =>
        List.productWithFoldLeft(input) shouldBe product
      }
    }
  }

  "3.12" - {
    "should reverse with fold" in {
      val table = Table(
        ("input", "expected"),
        (Nil, Nil),
        (List(1), List(1)),
        (List(1, 2), List(2, 1)),
        (List(1, 2, 3), List(3, 2, 1))
      )

      forEvery(table) { (input, expected) =>
        List.reverseWithFold(input) shouldBe expected
      }
    }
  }

  "3.13" - {
    "should foldLeft2 from the right" in {
      val table =
        Table(("input", "sum"), (Nil, 0), (List(1), 1), (List(1, 2, 3), 6))

      forEvery(table) { (input, sum) =>
        List.foldLeft2(input, 0)(_ + _) shouldBe sum
      }
    }

    "should foldRight2 from the right" in {
      val table =
        Table(("input", "sum"), (Nil, 0), (List(1), 1), (List(1, 2, 3), 6))

      forEvery(table) { (input, sum) =>
        List.foldRight2(input, 0)(_ + _) shouldBe sum
      }
    }
  }

  "3.14" - {
    "should append with fold" in {
      val table = Table(
        ("left", "right", "expected"),
        (Nil, Nil, Nil),
        (List(1), Nil, List(1)),
        (Nil, List(1), List(1)),
        (List(1), List(2), List(1, 2)),
        (List(1, 2), List(3, 4, 5), List(1, 2, 3, 4, 5))
      )

      forEvery(table) { (left, right, expected) =>
        List.append2(left, right) shouldBe expected
      }
    }
  }

  "3.15" - {
    "should flatten a nested list" in {
      val table = Table(
        ("input", "expected"),
        (Nil, Nil),
        (List(List(1)), List(1)),
        (List(List(1), Nil), List(1)),
        (List(List(1), List(2, 3)), List(1, 2, 3))
      )

      forEvery(table) { (input, expected) =>
        List.flattenWithFold(input) shouldBe expected
      }
    }
  }

  "3.16" - {
    "should add 1 to all entries in the list" in {
      val table = Table(
        ("input", "expected"),
        (Nil, Nil),
        (List(1), List(2)),
        (List(1, 2, 3), List(2, 3, 4))
      )

      forEvery(table) { (input, expected) =>
        List.addOne(input) shouldBe expected
      }
    }
  }

  "3.17" - {
    "should map list of doubles to list of strings" in {
      val table = Table(
        ("input", "expected"),
        (Nil, Nil),
        (List(1.0), List("1.0")),
        (List(1.0, 5.5), List("1.0", "5.5"))
      )

      forEvery(table) { (input, expected) =>
        List.stringifyDoubles(input) shouldBe expected
      }
    }
  }

  "3.18" - {
    "should map all values" in {
      val table = Table(
        ("input", "expected"),
        (Nil, Nil),
        (List(1), List(2)),
        (List(1, 2, 3), List(2, 3, 4))
      )

      forEvery(table) { (input, expected) =>
        List.map(input)(_ + 1) shouldBe expected
      }
    }
  }

  "3.19" - {
    "should filter matching values" in {
      val table = Table(
        ("input", "expected"),
        (Nil, Nil),
        (List(1), Nil),
        (List(1, 2, 3), List(2))
      )

      forEvery(table) { (input, expected) =>
        List.filter(input)(_ % 2 == 0) shouldBe expected
      }
    }
  }

  "3.20" - {
    "should flatMap" in {
      val table = Table(
        ("input", "expected"),
        (Nil, Nil),
        (List(1), List(1, 2)),
        (List(1, 2), List(1, 2, 2, 3))
      )

      forEvery(table) { (input, expected) =>
        List.flatMap(input)(v => List(v, v + 1)) shouldBe expected
      }
    }
  }

  "3.21" - {
    "should filter with flatMap" in {
      val table = Table(
        ("input", "expected"),
        (Nil, Nil),
        (List(1), Nil),
        (List(1, 2, 3), List(2))
      )

      forEvery(table) { (input, expected) =>
        List.filter2(input)(_ % 2 == 0) shouldBe expected
      }
    }
  }

  "3.22" - {
    "should sum lists of ints together" in {
      val table = Table(
        ("a", "b", "expected"),
        (Nil, Nil, Nil),
        (List(1), Nil, Nil),
        (Nil, List(1), Nil),
        (List(1, 2, 3), List(4, 5, 6), List(5, 7, 9))
      )

      forEvery(table) { (a, b, expected) =>
        List.zipInts(a, b) shouldBe expected
      }
    }
  }

  "3.23" - {
    "should generalize zipping" in {
      val table = Table(
        ("a", "b", "expected"),
        (Nil, Nil, Nil),
        (List(1), Nil, Nil),
        (Nil, List(1), Nil),
        (List(1, 2, 3), List(4, 5, 6), List(5, 7, 9))
      )

      forEvery(table) { (a, b, expected) =>
        List.zipWith(a, b)(_ + _) shouldBe expected
      }
    }
  }

  "3.24" - {
    "should find subsequences" in {
      val table = Table("subs", Nil, List(1, 2), List(2, 3), List(4))

      forEvery(table) { subs =>
        List.hasSubsequence(List(1, 2, 3, 4), subs) shouldBe true
      }
    }

    "should fail on misleading sequences" in {
      List.hasSubsequence(List(1, 2, 1, 4), List(1, 1)) shouldBe false
    }

    "should handle repeated beginning" in {
      List.hasSubsequence(List(1, 1, 1, 2, 3), List(1, 1, 2)) shouldBe true
    }
  }
}
