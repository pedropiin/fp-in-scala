object Main {
    def fib(n: Int): Int = {
        @annotation.tailrec
        def loop(n: Int, prev: Int, curr: Int): Int = {
            n match {
                case 0 => prev
                case _ => loop(n - 1, curr, prev + curr)
            }
        }
    
        loop(n, 0, 1)
    }

    
    def main(args: Array[String]): Unit = {
        println(fib(3))
    }
}
