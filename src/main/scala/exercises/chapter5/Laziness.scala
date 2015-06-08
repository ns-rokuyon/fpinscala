package exercises.chapter5

/**
 * Created by ns64 on 2015/06/03.
 */
trait Stream[+A] {
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
