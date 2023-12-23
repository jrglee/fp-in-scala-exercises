package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter14._
import org.scalatest.Inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor2}

import scala.collection.mutable

class Chapter14Spec extends AnyFreeSpec with Matchers with Inside with TableDrivenPropertyChecks {

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

  "14.3" - {
    "should create empty" in {
      ST.runST(new RunnableST[(Map[Int, Int], Int)] {
        override def apply[S]: ST[S, (Map[Int, Int], Int)] = for {
          m <- STMap.empty[S, Int, Int]
          size <- m.size
          res <- m.freeze
        } yield (res, size)
      }) shouldEqual (Map.empty, 0)
    }

    "should create from iterable" in {
      ST.runST(new RunnableST[(Map[Int, Int], Int, Option[Int])] {
        override def apply[S]: ST[S, (Map[Int, Int], Int, Option[Int])] = for {
          m <- STMap(1 -> 2)
          size <- m.size
          res <- m.freeze
          singleVal <- m.get(1)
        } yield (res, size, singleVal)
      }) shouldEqual (Map(1 -> 2), 1, Some(2))
    }

    "should create from map" in {
      ST.runST(new RunnableST[(Map[Int, Int], Int, Option[Int])] {
        override def apply[S]: ST[S, (Map[Int, Int], Int, Option[Int])] = for {
          m <- STMap.fromMap(Map(2 -> 3))
          size <- m.size
          res <- m.freeze
          singleVal <- m.get(2)
        } yield (res, size, singleVal)
      }) shouldEqual (Map(2 -> 3), 1, Some(3))
    }

    "should append and remove" in {
      ST.runST(new RunnableST[(Map[Int, Int], Int)] {
        override def apply[S]: ST[S, (Map[Int, Int], Int)] = for {
          m <- STMap(1 -> 2)
          _ <- m += (2 -> 3)
          _ <- m ++= Map(3 -> 4, 4 -> 5, 5 -> 6)
          _ <- m -= 1
          _ <- m --= List(4, 5)
          res <- m.freeze
          size <- m.size
        } yield (res, size)
      }) shouldEqual (Map(2 -> 3, 3 -> 4), 2)
    }
  }

}
