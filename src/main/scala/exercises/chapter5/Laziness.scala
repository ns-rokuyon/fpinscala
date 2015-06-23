package exercises.chapter5

/**
 * Created by ns64 on 2015/06/03.
 */
trait Stream[+A] {
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }
  // exercise5.1
  // StreamをListに変換し,強制的にStreamを評価する関数toList
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h()::t().toList
  }

  // exercise5.2
  // Streamの先頭からn個の要素を取り出す関数take(n)と
  // 先頭からn個の要素をスキップする関数drop(n)
  def take(n: Int): Stream[A] = this match {
    case Empty => Stream.empty
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => Stream.cons(h(), Stream.empty)
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // exercise5.3
  // Streamの先頭から指定された述語とマッチする要素をすべて取り出すtakeWhile関数
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  // exercise5.4
  // Streamの要素のうち指定された述語とマッチするものをすべてチェックするforAll
  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a,b) => p(a) && b)
  }

  // exercise5.5
  // foldRightを使ってtakeWhileを実装
  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((a,b) => if (p(a)) Stream.cons(a,b) else Stream.empty)
  }

  // exercise5.7
  // foldRightを使ってmap,filter,append,flatMapを実装
  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((a,b) => Stream.cons(f(a),b))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((a,b) => if (f(a)) Stream.cons(a,b) else b)
  }

  def append[B>:A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((a,b) => Stream.cons(a,b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((a,b) => f(a).append(b))
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
