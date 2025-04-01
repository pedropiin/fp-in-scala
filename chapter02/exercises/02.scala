object Exercise_2 {
  def isSorted[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= arr.length) true
      else if (!ordered(arr(n - 1), (arr(n)))) false
      else loop(n + 1)
    }

    loop(1)
  }

  def main(args: Array[String]): Unit = {
    val tests: Array[Array[Int]] = Array(Array(1, 2, 3, 4, 7, 6, 9, 4), Array(0), Array(1, 2, 3, 4, 5, 6))
    tests.map(isSorted(_, (a: Int, b: Int) => if (a < b) true else false)).map(println(_))
  }
}
