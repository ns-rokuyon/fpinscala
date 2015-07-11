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
  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

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

  // exercise6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = RNG.double(r)
    ((i,d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i,d), r) = RNG.intDouble(rng)
    ((d,i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = RNG.double(rng)
    val (d2, r2) = RNG.double(r1)
    val (d3, r3) = RNG.double(r2)
    ((d1, d2, d3), r3)
  }

  // exercise6.4
  // ランダムな整数のリストを生成する関数
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (Nil, rng)
    case _ => {
      val (x1, r1) = rng.nextInt
      val (x2, r2) = ints(count - 1)(r1)
      (x1::x2, r2)
    }
  }

  // exercise6.5
  // mapを使ってdoubleを再実装
  def _double: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  // exercise6.6
  // map2の実装
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }
  }
}
