/**
 * Created by ns64 on 2015/05/04.
 */
import exercises.chapter2.GettingStarted
import exercises.chapter3

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


    // chapter3 ---------------------

    // 3.1
    println(chapter3.List.x)

    // 3.2
    println(chapter3.List.tail(chapter3.List(1,2,3,4,5)))
    println(chapter3.List.tail(chapter3.List(1,2)))
    println(chapter3.List.tail(chapter3.List(1)))
    println(chapter3.List.tail(chapter3.List(chapter3.Nil)))

    // 3.3
    println(chapter3.List.setHead(chapter3.List(1,2,3,4,5), 10))
    println(chapter3.List.setHead(chapter3.List(1), 10))
    println(chapter3.List.setHead(chapter3.List(chapter3.Nil), 10))

    // 3.4
    println(chapter3.List.drop(chapter3.List(1,2,3,4,5), 3))
    println(chapter3.List.drop(chapter3.List(4,5), 3))

    // 3.5
    println(chapter3.List.dropWhile(chapter3.List(1,2,3,4,5), (x: Int) => x < 3))
    println(chapter3.List.dropWhile(chapter3.List(1,2,3,4,5), (x: Int) => x > 0))
    println(chapter3.List.dropWhile(chapter3.List(1,2,3,4,5), (x: Int) => x == 0))

    // 3.6
    println(chapter3.List.init(chapter3.List(1,2,3,4)))
    println(chapter3.List.init(chapter3.List(1)))
    println(chapter3.List.init(chapter3.Nil))

    // 3.9
    println(chapter3.List.length(chapter3.List(1,2,3,4)))
    println(chapter3.List.length(chapter3.List(1,2,3,4,5,6,7,8,9,10)))
    println(chapter3.List.length(chapter3.Nil))

    // 3.10, 3.11
    println(chapter3.List.sumFoldL(chapter3.List(1,2,3,4,5,6,7,8,9,10)))
    println(chapter3.List.productFoldL(chapter3.List(1,2,3,4,5)))
    println(chapter3.List.lengthFoldL(chapter3.List(1,2,3,4)))
    println(chapter3.List.lengthFoldL(chapter3.List(1,2,3,4,5,6,7,8,9,10)))
    println(chapter3.List.lengthFoldL(chapter3.Nil))

    // 3.12
    println(chapter3.List.reverse(chapter3.List(1,2,3,4,5,6,7,8,9,10)))

    // 3.14
    println(chapter3.List.append(chapter3.List(1,2,3,4,5,6,7,8,9,10),chapter3.List(11,12)))

    // 3.15
    println(chapter3.List.concat(chapter3.List(chapter3.List(1,2,3,4,5), chapter3.List(6,7,8,9,10))))

    // 3.16
    println(chapter3.List.addone(chapter3.List(1,2,3,4)))

    // 3.17
    println(chapter3.List.doubleToString(chapter3.List(1.1,2.2,3.3,4.4)))

    // 3.18
    println(chapter3.List.map(chapter3.List(1,2,3,4))(plus10))

    // 3.19
    println(chapter3.List.filter(chapter3.List(1,2,3,4,5,6,7,8,9,10))((x: Int) => if (x % 2 == 0) true else false))

    // 3.20
    println(chapter3.List.flatMap(chapter3.List(1,2,3))(i => chapter3.List(i,i)))

    // 3.21
    println(chapter3.List.filter2(chapter3.List(1,2,3,4,5,6,7,8,9,10))((x: Int) => if (x % 2 == 0) true else false))

    // 3.22
    println(chapter3.List.add(chapter3.List(1,2,3), chapter3.List(4,5,6)))

    // 3.23
    println(chapter3.List.zipWith(chapter3.List(1,2,3), chapter3.List(4,5,6))((x, y) => x - y))
  }
}
