package exercises.chapter6

/**
 * Created by ns64 on 2015/07/03.
 */
trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  // exercise6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    (if (n < 0) -(n + 1) else n, r)
  }

  // exercise6.2
  // 0-1のDouble型の値を生成する関数
  def double(rng: RNG): (Double, RNG) = {
    val (n, r) = rng.nextInt
    (n / (Int.MaxValue.toDouble + 1), r)
  }
}
