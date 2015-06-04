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
