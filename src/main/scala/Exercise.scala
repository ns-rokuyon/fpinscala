/**
 * Created by ns64 on 2015/05/04.
 */
import exercises.chapter2.GettingStarted

object Exercise {
  def main(args: Array[String]): Unit ={
    println("exercise")
    printf("%d", GettingStarted.fib(6))
    println("")

    def ordered(a: Int, b: Int): Boolean = {
      a < b
    }
    println(GettingStarted.isSorted(Array(1,2,5,4), ordered))

    def plus(a: Int, b: Int): Int = {
      a + b
    }
    val curry_plus = GettingStarted.curry(plus)
    val plus2 = curry_plus(2)
    println(plus2(10))

    val uncurry_plus = GettingStarted.uncurry(curry_plus)
    println(uncurry_plus)
    println(uncurry_plus(2,10))

    val plus10 = curry_plus(10)
    println(GettingStarted.compose(plus2, plus10)(100))

  }
}
