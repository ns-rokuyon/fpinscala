package exercises.chapter3

/**
 * Created by ns64 on 2015/05/05.
 */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List{
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  // exercise3.1
  // パターンマッチ
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }   // -> 3

  // exercise3.2
  // Listの最初の要素を削除する関数tail
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  // exercise3.3
  // Listの最初の要素を別の値と置き換える関数setHead
  def setHead[A](l: List[A], a: A) = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(a, t)
  }

  // exercise3.4
  // リストの先頭からn個の要素を削除する関数drop
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (l, n) if n <= 0 => l
    case (Nil, n) => Nil
    case (Cons(h,t), n) => drop(t, n-1)
  }

  // exercise3.5
  // 述語にマッチする場合に限り,Listからその要素までの要素を削除する関数dropWhile
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => f(h) match {
        case false => l
        case true => dropWhile(t, f)
      }
    }
  }

  // exercise3.6
  // Listの末尾を除く全ての要素で構成されたListを返す関数init
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // list3-3
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)
  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  // exercise3.9
  // foldRightを使ってリストの長さを計算せよ
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, y) => 1 + y)

  // exercise3.10
  // 関数foldLeft
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
  }

  // exercise3.11
  // foldLeftを使ったsum,product,lengthの実装
  def sumFoldL(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def productFoldL(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
  def lengthFoldL[A](as: List[A]): Int = foldLeft(as, 0)((x, _) => 1 + x)

  // exercise3.12
  // 要素が逆に並んだリストを返す関数
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((xs, x) => Cons(x, xs))
  // List[A]がNilだとダメ

  // exercise3.14
  // リストの末尾に他のリストを付け加える関数append
  def append[A](l: List[A], a: List[A]): List[A] = foldRight(l, a)((x,xs) => Cons(x,xs))

  // exercise3.15
  // 複数のリストからなるリストを1つのリストに結合する関数
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

  // exercise3.16
  // リストの各値に+1したリストを返す関数
  def addone(l :List[Int]): List[Int] = foldRight(l, List[Int]())((x, xs) => Cons(x+1, xs))

  // exercise3.17
  // Doubleのリストの各値をStringに変換した関数
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, List[String]())((x, xs) => Cons(x.toString, xs))

  // exercise3.18
  // リストの各要素を変更しリストの構造をそのまま保つ総称関数map
  def map[A,B](as: List[A])(f : A => B): List[B] = foldRight(as, List[B]())((h, t) => Cons(f(h), t))

  // exercise3.19
  // 述語が満たされるまでリストから要素を削除する関数filter
  // この関数を使ってList[Int]から奇数をすべて削除せよ
  // (述語が満たされるまで...は誤植?)
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((h, t) => if(f(h)) Cons(h, t) else t)

  // exercise3.20
  // flatMap
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  // exercise3.21
  // flatMapを使ってfilterを実装せよ
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a  => if (f(a)) List(a) else Nil)

  // exercise3.22
  // リストを2つ受け取り対応する要素同士を足しあわせて新しいリストを生成する関数
  def add(xlist: List[Int], ylist: List[Int]): List[Int] = (xlist, ylist) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, add(xs, ys))
  }

  // exercise3.23
  // 3.22の一般化
  def zipWith[A,B,C](xlist: List[A], ylist: List[B])(f: (A, B) => C): List[C] = (xlist, ylist) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }
}
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // exercise3.25
  // 2分木のノードの数を数える関数size
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(a) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }
}

