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

  // exercise5.13
  // unfoldを使ってmap,take,takeWhile,zipWith,zipAllを実装
  def map2[B](f: A => B): Stream[B] = {
    Stream.unfold(this) {
      case Cons(h,t) => Some((f(h()) ,t()))
      case _ => None
    }
  }

  def take2(n: Int): Stream[A] = Stream.unfold((n,this)) {
    case ((1, Cons(h,t))) => Some((h(), (0, Stream.empty)))
    case ((x, Cons(h,t))) if n > 1 => Some((h(), (x-1, t())))
    case _ => None
  }

  def takeWhile3(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h,t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B,C](s: Stream[B])(f: (A,B) => C): Stream[C] = {
    Stream.unfold((this,s)) {
      case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(),h2()),(t1(),t2())))
      case _ => None
    }
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

  def ones: Stream[Int] = Stream.cons(1, ones)

  // exercise5.8
  // onesを一般化し指定された値の無限ストリームを生成するconstant
  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, constant(a))
  }

  // exercise5.9
  // nで始まってn+1, n+2と続く無限ストリームを生成
  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n+1))
  }

  // exercise5.10
  // フィボナッチ数列の無限ストリーム
  def fibs: Stream[Int] = {
    def loop(a: Int, b: Int): Stream[Int] = {
      Stream.cons(a, loop(b, a+b))
    }
    loop(0,1)
  }

  // exercise5.11
  // 汎用的なストリーム生成関数unfold
  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = {
    f(z) match {
      case Some((x,y)) => Stream.cons(x, unfold(y)(f))
      case None => Stream.empty
    }
  }

  // exercise5.12
  // unfoldを使ってfibs,from,constant,ones
  def ones2: Stream[Int] = unfold(1)(s => Some((1,1)))

  def from2(n: Int): Stream[Int] = unfold(n)(s => Some((s,s+1)))

  def constant2[A](a: A): Stream[A] = unfold(a)(s => Some((s,s)))

  def fibs2: Stream[Int] = unfold((0,1))(s => Some((s._1, (s._2, s._1 + s._2))))
}
