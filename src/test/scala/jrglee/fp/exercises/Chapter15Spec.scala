package jrglee.fp.exercises

import jrglee.fp.exercises.Chapter05.Stream
import jrglee.fp.exercises.Chapter13.IO
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.ForkJoinPool
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

class Chapter15Spec extends AnyFreeSpec with Matchers {

  {
    import Chapter15.Simple._
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

    "15.2" - {
      "should count elements" in {
        Process.count(Stream("a", "b", "c")).toList shouldEqual List(1, 2, 3)
      }
    }

    "15.3" - {
      "should emit running average" in {
        Process.mean(Stream(2, 4, 6)).toList shouldEqual List(2d, 3d, 4d)
      }
    }

    "15.4" - {
      "should count elements" in {
        Process.count2(Stream("a", "b", "c")).toList shouldEqual List(1, 2, 3)
      }

      "should emit running average" in {
        Process.mean2(Stream(2, 4, 6)).toList shouldEqual List(2d, 3d, 4d)
      }
    }

    "15.5" - {
      "should chain processes" in {
        (Process.take(2) |> Process.sum)(Stream(1, 2, 3, 4)).toList shouldEqual List(1d, 3d)
      }
    }

    "15.6" - {
      "should zip with index" in {
        Process.lift[String, String](identity).zipWithIndex(Stream("a", "b", "c")).toList shouldEqual List(
          "a" -> 1,
          "b" -> 2,
          "c" -> 3
        )
      }
    }

    "15.7" - {
      "should generically combine with zip" in {
        Process.lift[String, String](identity).zip(Process.count2)(Stream("a", "b", "c")).toList shouldEqual List(
          "a" -> 1,
          "b" -> 2,
          "c" -> 3
        )
      }

      "should calculate mean with zip" in {
        Process.mean3(Stream(2, 4, 6)).toList shouldEqual List(2d, 3d, 4d)
      }
    }

    "15.8" - {
      "should run predicate" in {
        Process.exists[Int](_ == 5)(Stream(1, 2, 3, 4, 5, 6)).toList shouldEqual List(false, false, false, false, true)
      }
    }

    "15.9" - {
      "should convert entries from fahrenheit to celsius" in {
        fahrenheitToCelsius(Stream("32", "# nope", "212", "", "-40")).toList shouldEqual List("0.0", "100.0", "-40.0")
      }
    }
  }

  {
    import Chapter15.Generic._

    "15.10" - {
      "should run log with monad" in {
        val task = Task.unit(10).flatMap(v => Task.unit(v + 2))

        val p: Process[Task, Int] = Process.await(task) {
          case Right(value) => Process.emit(value)
          case Left(e)      => Process.Halt(e)
        }

        val log = p.runLog.run(ForkJoinPool.commonPool())
        log shouldEqual List(12)
      }
    }

    "15.11" - {
      "should eval value" in {
        val log = Process.eval(Task.unit(10)).runLog.run(ForkJoinPool.commonPool())
        log shouldEqual List(10)
      }

      "should eval_ and discard" in {
        val called = new AtomicBoolean(false)
        val task: Task[Unit] = Task(IO(Right(called.set(true))))
        val log = Process.eval(task).runLog.run(ForkJoinPool.commonPool())

        log should have size 1
        called.get() shouldBe true
      }
    }

    "15.12" - {
      "should join" in {
        val value = new AtomicInteger(0)
        val sink: Process.Sink[Task, Int] =
          Process.eval(Task(IO(Right(v => Process.eval(Task(IO(Right(value.set(v)))))))))
        val p = Process.join(Process.eval(Task.unit(10)).zipWith(sink)((o, f) => f(o)))

        val log = p.runLog.run(ForkJoinPool.commonPool())
        log should have size 1

        value.get() shouldBe 10
      }
    }
  }
}
