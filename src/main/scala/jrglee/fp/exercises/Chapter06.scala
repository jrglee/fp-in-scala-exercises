package jrglee.fp.exercises

object Chapter06 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5deece66dL) + 0xbL & 0xffffffffffL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  case class FixedValueRNG(value: Int) extends RNG {
    override def nextInt: (Int, RNG) = (value, this)
  }

  case class IncrementalValueRNG(value: Int) extends RNG {
    override def nextInt: (Int, RNG) = (value, IncrementalValueRNG(value + 1))
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt
    (n & Int.MaxValue, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = nonNegativeInt(rng)
    (n.toDouble / Int.MaxValue, nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (n, rng3) = rng2.nextInt
    ((d, n), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = (1 to count).foldLeft((List.empty[Int], rng)) {
    case ((acc, currentRng), _) =>
      val (v, nextRng) = currentRng.nextInt
      (v +: acc, nextRng)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = { rng =>
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def doubleWithMap(rng: RNG): (Double, RNG) = map(nonNegativeInt)(_.toDouble / Int.MaxValue)(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def sequence[A](a: List[Rand[A]]): Rand[List[A]] = { rng =>
    val (randomAs, finalRnd) = a.foldLeft((List.empty[A], rng)) { case ((lst, currentRng), rand) =>
      val (v, nextRng) = rand(currentRng)
      (v +: lst, nextRng)
    }
    (randomAs.reverse, finalRnd)
  }

  def intsWithSequence(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)(int))(rng)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (a, nextRng) = f(rng)
    g(a)(nextRng)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) { rng => (mod, rng) }
    else nonNegativeLessThan(n)
  }

  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  case class State[S, +A](run: S => (A, S)) {
    def flatMap[B](f: A => State[S, B]): State[S, B] = State { state =>
      val (a, nextState) = run(state)
      f(a).run(nextState)
    }

    def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for {
        a <- sa
        b <- sb
      } yield f(a, b)

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State { state =>
      val (ret, finalState) = fs.foldLeft((List.empty[A], state)) { case ((acc, currentState), combinator) =>
        val (v, nextState) = combinator.run(currentState)
        (v +: acc, nextState)
      }
      (ret.reverse, finalState)
    }
  }

  type RandState[A] = State[RNG, A]
  object RandState {

    def int: RandState[Int] = State(_.nextInt)

    def unit[A](a: A): RandState[A] = State.unit(a)

  }
}
