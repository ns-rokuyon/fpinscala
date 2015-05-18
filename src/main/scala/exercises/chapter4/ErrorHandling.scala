package exercises.chapter4

/**
 * Created by ns64 on 2015/05/18.
 */
sealed trait Option[+A] {

  // Exercise4.1
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

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
