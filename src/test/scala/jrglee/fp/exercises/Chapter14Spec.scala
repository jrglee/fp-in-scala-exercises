package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter14._
import org.scalatest.Inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Chapter14Spec extends AnyFreeSpec with Matchers with Inside {

  "14.1" - {
    "should not change on empty" in {
      val p = new RunnableST[List[Int]] {
        override def apply[S]: ST[S, List[Int]] = for {
          arr <- STArray(6, 0)
          _ <- arr.fill(Map.empty)
          res <- arr.freeze
        } yield res
      }

      val res = ST.runST(p)
      res shouldEqual List.fill(6)(0)
    }

    "should add items" in {
      val p = new RunnableST[List[Int]] {
        override def apply[S]: ST[S, List[Int]] = for {
          arr <- STArray(6, 0)
          _ <- arr.fill(Map(1 -> 2, 3 -> 4, 5 -> 6))
          res <- arr.freeze
        } yield res
      }

      val res = ST.runST(p)
      res shouldEqual List(0, 2, 0, 4, 0, 6)
    }
  }

  "14.2" - {
    "should partition" in {
      val p = new RunnableST[(Int, List[Int])] {
        override def apply[S]: ST[S, (Int, List[Int])] = for {
          arr <- STArray.fromList(List(4, 3, 2, 1, 5))
          pi <- partition(arr, 0, 4, 1)
          res <- arr.freeze
        } yield (pi, res)
      }

      val (pi, res) = ST.runST(p)
      pi shouldBe 2
      res(2) shouldBe 3
      all(res.slice(0, 2)) should be < 3
      all(res.slice(3, 2)) should be > 3
    }

    "should sort with partition" in {
      val p = new RunnableST[List[Int]] {
        override def apply[S]: ST[S, List[Int]] = for {
          arr <- STArray.fromList(List(4, 3, 2, 1, 5))
          _ <- qs(arr, 0, 4)
          res <- arr.freeze
        } yield res
      }

      val res = ST.runST(p)
      res shouldEqual List(1, 2, 3, 4, 5)
    }
  }

}
