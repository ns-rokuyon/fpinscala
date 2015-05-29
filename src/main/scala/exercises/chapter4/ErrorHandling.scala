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

  def Try[A](a: A): Option[A] = try Some(a) catch { case e: Exception => None }

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

  // exercise4.4
  // Optionのリストを1つのOptionにまとめる関数sequence
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case x::xs => x.flatMap(x1 => sequence(xs).map(x1::_))
  }

  // exercise4.5
  // traverse関数
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case x::xs => map2(f(x),traverse(xs)(f))(_::_)
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Either[+E, +A] {
  // exercise4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(a => b map (b1 => f(a, b1)))
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  // exercise4.7
  // Eitherでsequenceとtraverseを実装
  def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h::t => f(h).map2(traverse(t)(f))(_::_)
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E, List[A]] = {
    traverse(es)(a => a)
  }
}
