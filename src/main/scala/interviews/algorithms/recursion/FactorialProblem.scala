package interviews.algorithms.recursion

import scala.annotation.tailrec

object FactorialProblem extends App {

  def factorial(n: BigInt): BigInt = {

    /*
        factorial(3) = factorialTailRec(3, 1)
          = factorialTailRec(2, 3 * 1)
          = factorialTailRec(1, 2 * 3 * 1)
          = 2 * 3 * 1

        Complexity = O(i)
     */
    @tailrec
    def factorialTailRec(i: BigInt, accumulator: BigInt): BigInt = {
      if(i == 1)  accumulator
      else factorialTailRec(i-1, i * accumulator)
    }
    factorialTailRec(n, 1)
  }

  println(factorial(5))
}
