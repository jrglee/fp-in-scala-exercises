package jrglee.fp.exercises

object Chapter08 {

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

  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }

  object Prop {
    type FailedCase = String
    type SuccessCount = Int

    def check(p: => Boolean): Prop = Prop { (_, _, _) =>
      if (p) Passed else Falsified("()", 0)
    }
  }

  type MaxSize = Int
  type TestCases = Int
  case class Prop(run: (MaxSize, TestCases, Chapter06.RNG) => Result) {
    def &&(p: Prop): Prop = Prop { (max, n, rng) =>
      run(max, n, rng) match {
        case Passed       => p.run(max, n, rng)
        case f: Falsified => f
      }
    }

    def ||(p: Prop): Prop = Prop { (max, n, rng) =>
      run(max, n, rng) match {
        case Passed       => Passed
        case _: Falsified => p.run(max, n, rng)
      }
    }
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
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

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    val casesPerSize = (n + (max - 1)) / max
    val props: LazyList[Prop] = LazyList.from(0).take(n.min(max) + 1).map(i => forAll(g(i))(f))
    val prop: Prop = props.map(p => Prop { (max, _, rng) => p.run(max, casesPerSize, rng) }).toList.reduce(_ && _)
    prop.run(max, n, rng)
  }

  def run(
    p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: Chapter06.RNG = Chapter06.SimpleRNG(System.currentTimeMillis)
  ): Unit = p.run(maxSize, testCases, rng) match {
    case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
    case Passed            => println(s"+ OK, passed $testCases tests.")
    case Proved            => println(s"+ OK, proved property.")
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

    def unsized: SGen[A] = SGen(n => this)
  }

  object Gen {
    def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(
      Chapter06.State(Chapter06.nonNegativeInt).map(n => start + n % (stopExclusive - start))
    )

    def unit[A](a: => A): Gen[A] = Gen(Chapter06.State(Chapter06.unit(a)))
    def boolean: Gen[Boolean] = Gen(Chapter06.State(Chapter06.map(Chapter06.nonNegativeInt)(_ % 2 == 0)))
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(Chapter06.State.sequence(List.fill(n)(g.sample)))
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(if (_) g1 else g2)
    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      val g1Fraction = g1._2 / (g1._2 + g2._2)
      Gen(Chapter06.State(Chapter06.map(Chapter06.double)(_ <= g1Fraction))).flatMap(if (_) g1._1 else g2._1)
    }
  }

  case class SGen[A](forSize: Int => Gen[A]) {
    def apply(n: Int): Gen[A] = forSize(n)

    def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n => forSize(n).flatMap(v => f(v).forSize(n)))
  }

  object SGen {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(listOf(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    val maxProp1 = forAll(listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    val sortedProp = forAll(listOf(smallInt)) { ns =>
      val l = ns.sorted
      l.isEmpty || l.tail.isEmpty || l.zip(l.tail).forall { case (a, b) => a <= b }
    }

    def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n, g))
    def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n.max(1), g))
  }
}
