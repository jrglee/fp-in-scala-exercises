package jrglee.fp.exercises

object Chapter08 {

  object Section2 {

    import Prop.{FailedCase, SuccessCount}
    sealed trait Result {
      def isFalsified: Boolean
    }

    case object Passed extends Result {
      override def isFalsified: Boolean = false
    }

    case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
      override def isFalsified: Boolean = true
    }

    object Prop {
      type FailedCase = String
      type SuccessCount = Int
    }

    type TestCases = Int
    case class Prop(run: (TestCases, Chapter06.RNG) => Result) {
      def &&(p: Prop): Prop = Prop { (n, rng) =>
        run(n, rng) match {
          case Passed       => p.run(n, rng)
          case f: Falsified => f
        }
      }

      def ||(p: Prop): Prop = Prop { (n, rng) =>
        run(n, rng) match {
          case Passed       => Passed
          case _: Falsified => p.run(n, rng)
        }
      }
    }

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
      randomStream(as)(rng)
        .zip(LazyList.from(0))
        .take(n)
        .map { case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
        }
        .find(_.isFalsified)
        .getOrElse(Passed)
    }

    def randomStream[A](g: Gen[A])(rng: Chapter06.RNG): LazyList[A] =
      LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

    def buildMsg[A](s: A, e: Exception): String =
      s"""test case: $s
         |generated an exception: ${e.getMessage}
         |stack trace
         |${e.getStackTrace.mkString("\n")}
         |""".stripMargin

    case class Gen[A](sample: Chapter06.State[Chapter06.RNG, A]) {
      def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(Chapter06.State({ currentState =>
        val (v, nextState) = sample.run(currentState)
        f(v).sample.run(nextState)
      }))

      def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(s => Gen.listOfN(s, this))
    }

    object Gen {
      def choose(start: Int, stopExclusive: Int): Gen[Int] = {
        def rangedInt: Chapter06.Rand[Int] = { rng =>
          val (i, state) = rng.nextInt
          if (i >= start && i < stopExclusive) (i, state)
          else rangedInt(state)
        }
        Gen(Chapter06.State(rangedInt))
      }

      def unit[A](a: => A): Gen[A] = Gen(Chapter06.State(Chapter06.unit(a)))
      def boolean: Gen[Boolean] = Gen(Chapter06.State(Chapter06.map(Chapter06.nonNegativeInt)(_ % 2 == 0)))
      def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(Chapter06.State.sequence(List.fill(n)(g.sample)))
      def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(if (_) g1 else g2)
      def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
        val g1Fraction = g1._2 / (g1._2 + g2._2)
        Gen(Chapter06.State(Chapter06.map(Chapter06.double)(_ <= g1Fraction))).flatMap(if (_) g1._1 else g2._1)
      }
    }
  }
}
