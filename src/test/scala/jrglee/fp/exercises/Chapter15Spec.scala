package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter15.Process
import jrglee.fp.exercises.Chapter05.Stream
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter15Spec extends AnyFreeSpec with Matchers {

  "15.1" - {
    "should take n values" in {
      Process.take(2)(Stream(1, 2, 3)).toList shouldEqual List(1, 2)
    }

    "should shortcut take with less than n values" in {
      Process.take(2)(Stream(1)).toList shouldEqual List(1)
    }

    "should drop n values" in {
      Process.drop(2)(Stream(1, 2, 3, 4)).toList shouldEqual List(3, 4)
    }

    "should shortcut drop with less than n values" in {
      Process.drop(2)(Stream(1)).toList shouldEqual List.empty
    }

    "should takeWhile with predicate" in {
      Process.takeWhile[Int](_ < 5)(Stream(1, 2, 3, 4, 5, 6)).toList shouldEqual List(1, 2, 3, 4)
    }

    "should dropWhile with predicate" in {
      Process.dropWhile[Int](_ < 5)(Stream(1, 2, 3, 4, 5, 6)).toList shouldEqual List(5, 6)
    }
  }
}
