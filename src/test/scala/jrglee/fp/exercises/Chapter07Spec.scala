package jrglee.fp.exercises

import org.scalatest.Inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ExecutorService, ForkJoinPool, TimeUnit}
import scala.concurrent.TimeoutException
import scala.util.Failure

class Chapter07Spec extends AnyFreeSpec with Matchers with Inside {
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

  "section 4" - {
    import Section4._

    "7.10" - {
      "should handle errors" in {
        inside(
          Par.run(executorService)(es =>
            new Future[Int] {
              override def apply(k: Int => Unit): Unit = {
                throw new RuntimeException("boom!")
              }
            }
          )
        ) { case Failure(exception) =>
          exception shouldBe a[RuntimeException]
          exception.getMessage shouldBe "boom!"
        }
      }
    }
  }

  "section 5" - {
    import Section5._
    "7.11" - {
      "should choose computation based on index" in {
        Par.run(executorService)(Par.choice(Par.unit(false))(Par.unit(5), Par.unit(10))) shouldBe 10
      }
    }

    "7.12" - {
      "should choose computation based on key" in {
        Par.run(executorService)(
          Par.choiceMap(Par.unit("foo"))(Map("foo" -> Par.unit(5), "bar" -> Par.unit(10)))
        ) shouldBe 5
      }
    }

    "7.13" - {
      "should choose computation with a late binding function" in {
        Par.run(executorService)(Par.chooser(Par.unit(5))(v => Par.unit(v + 2))) shouldBe 7
      }
    }

    "7.14" - {
      "should join with custom implementation" in {
        Par.run(executorService)(Par.join(Par.unit(Par.unit(10)))) shouldBe 10
      }

      "should join with flatMap" in {
        Par.run(executorService)(Par.joinWithFlatMap(Par.unit(Par.unit(10)))) shouldBe 10
      }

      "should flatMap with join" in {
        Par.run(executorService)(Par.flatMapWithJoin(Par.unit(2))(v => Par.unit(v + 5))) shouldBe 7
      }

      "should map2 with flatMap and unit" in {
        Par.run(executorService)(Par.map2(Par.unit(5), Par.unit(10))(_ + _)) shouldBe 15
      }
    }
  }
}
