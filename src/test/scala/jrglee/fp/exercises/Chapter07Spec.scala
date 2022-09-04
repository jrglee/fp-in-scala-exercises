package jrglee.fp.exercises

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ExecutorService, ForkJoinPool, TimeUnit}
import scala.concurrent.TimeoutException

class Chapter07Spec extends AnyFreeSpec with Matchers {
  import Chapter07._

  val executorService: ExecutorService = ForkJoinPool.commonPool()

  "section 3" - {
    import Section3._

    "7.3" - {
      "should still work like a map2" in {

        Par.map2WithTimeout(Par.unit(1), Par.unit("a"))((_, _))(executorService).get() shouldBe (1, "a")
      }

      "should timeout when a computation does not return at all" in {
        val sleepyComputation = Par.map2WithTimeout(
          Par.unit(1),
          Par.lazyUnit {
            Thread.sleep(TimeUnit.HOURS.toMillis(1)); 2
          }
        )(_ + _)(executorService)
        a[TimeoutException] should be thrownBy sleepyComputation.get(50, TimeUnit.MILLISECONDS)
      }
    }

    "7.4" - {
      "should apply transformation in async" in {
        val counter = new AtomicInteger(0)

        val asyncFn = Par.asyncF[Int, String] { v =>
          counter.incrementAndGet()
          v.toString
        }

        counter.get() shouldBe 0
        val parResult = asyncFn(10)

        counter.get() shouldBe 0
        parResult(executorService).get() shouldBe "10"
        counter.get() shouldBe 1
      }
    }

    "7.5" - {
      "should generate a sequence of parallel computations" in {
        Par.sequence(List(Par.unit(1), Par.unit(2)))(executorService).get() shouldEqual List(1, 2)
      }
    }

    "7.6" - {
      "should filter in parallel" in {
        Par.parFilter((1 to 10).toList)(_ % 2 == 0)(executorService).get() shouldEqual List(2, 4, 6, 8, 10)
      }
    }
  }
}
