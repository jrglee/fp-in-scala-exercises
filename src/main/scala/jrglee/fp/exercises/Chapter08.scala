package jrglee.fp.exercises

object Chapter08 {

  object Section2 {

    object Prop {
      type FailedCase = String
      type SuccessCount = Int
    }

    trait Prop {
      import Prop._

      def check: Either[(FailedCase, SuccessCount), SuccessCount]

//      def &&(p: Prop): Prop = if (check) p else this
    }

    def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

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
    }
  }
}
