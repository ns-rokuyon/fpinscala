package exercises.chapter2

/**
 * Created by ns64 on 2015/05/04.
 */
object GettingStarted {

  // exercise2.1
  // 末尾再帰関数
  def fib(n: Int): Int = {
    def loop(n :Int, p: Int, c: Int): Int = {
      n match {
        case 0 => p
        case _ => loop(n-1, c, p+c)
      }
    }
    loop(n, 0, 1)
  }

  // exercise2.2
  // 多相関数
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    val length = as.length
    def loop(n: Int): Boolean = {
      if ( n >= length - 1 ) true
      else
        if ( ordered(as(n), as(n+1)) ) loop(n+1)
        else false
    }
    loop(0)
  }

  // exercise2.3
  // カリー化
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  // exercise2.4
  // カリー化の逆
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  // exercise2.5
  // 合成関数
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

}
