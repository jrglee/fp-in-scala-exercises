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
}
