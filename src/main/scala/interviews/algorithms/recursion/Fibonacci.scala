package interviews.algorithms.recursion

import scala.annotation.tailrec

object Fibonacci extends App {

  def fibonacciNumber(n: BigInt): BigInt = {

    /*
       fibonacci series : 0,1,1,2,3,5,8,13,21

       fibonacciNumber(6) = fibonacciNumberTailRec(6,0,1)
         = fibonacciNumberTailRec(5, 1, 1)
         = fibonacciNumberTailRec(4, 1, 2)
         = fibonacciNumberTailRec(3, 2, 3)
         = fibonacciNumberTailRec(2, 3, 5)
         = 5

       Complexity: O(2^N) Exponential
     */

    @tailrec
    def fibonacciNumberTailRec(i: BigInt, first: BigInt, second: BigInt): BigInt = {
      if(i == 2) second
      else fibonacciNumberTailRec(i - 1, second, first + second)
    }

    if(n < 2) n
    else fibonacciNumberTailRec(n,0,1)
  }

  println(fibonacciNumber(10000))

}
