package exercises.chapter4

/**
 * Created by ns64 on 2015/05/18.
 */
sealed trait Option[+A] {

  // exercise4.1
  // Optionを使ってmap,flatMap,getOrElse,orElse,filterを実装
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(x) => f(x)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(x) => if (f(x)) this else None
  }
}

object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  // exercise4.2
  // flatMapをベースに分散を返す関数variance
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m,2))))
  }

  // exercise4.3
  // Option型の2つの値を結合する総称関数map2
  // どちらかのOption値がNoneの場合は戻り値もNone
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(a1 => b.map(b1 => f(a1, b1)))
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
