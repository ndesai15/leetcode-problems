package com.rockthejvm.numbers

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-01-17
  *
 */

object NumberOps{
  implicit class RRichInt(n: Int) { // our rich int

    /**
      *  Easy Problems
      */
    def isPrime: Boolean = {

      /*
        isPrime(25) = isPrimeTailRec(2)
          = isPrimeTailRec(3)
          = isPrimeTailRec(4)
          = isPrimeTailRec(5)
          = false

        isPrime(7) = isPrimeTailRec(2)
          = isPrimeTailRec(3)
          = true

        Complexity: O(sqrt(n))

       */
      @tailrec
      def isPrimeTailRec(i: Int): Boolean = {
        if(i > Math.sqrt(Math.abs(n))) true
        else (n % i != 0) && isPrimeTailRec(i+1)
      }
      if(n == 0 || n == -1 || n == 1) false
      else isPrimeTailRec(2)
    }

    // the constituent prime divisors
    def decompose: List[Int] = {
      assert(n>0)

      /*
         decompose(20) = decomposeTailRec(20, 2, List())
           = decompose(10, 2,List(2))
           = decompose(5, 2, List(2,2))
           = decompose(5, 3, List(2,2))
           = decompose(5, 3, List(2,2))
           = decompose(5, 4, List(2,2))
           = List(5,2,2)

         decompose(16) = decomposeTailRec(16, 2, [])
           = decomposeTailRec(8, 2, [2])
           = decomposeTailRec(4, 2, [2,2])
           = decomposeTailRec(2, 2, [2,2,2])
           = [2,2,2,2]

         Complexity: O(sqrt(N)); can be as low as O(log(N))

      */

      @tailrec
      def decomposeTailRec(remaining: Int, currentDivisor: Int, accumulator: List[Int]): List[Int] = {
        if(currentDivisor > Math.sqrt(remaining)) remaining :: accumulator // ??
        else if(remaining % currentDivisor == 0) decomposeTailRec(remaining/currentDivisor, currentDivisor, currentDivisor :: accumulator)
        else decomposeTailRec(remaining, currentDivisor+1, accumulator)
      }
      decomposeTailRec(n, 2, List())
    }
  }
}

object NumberProblems extends App {

  import NumberOps._
  def testIsPrime() = {
    println(2.isPrime)
    println(15.isPrime)
    println(2003.isPrime)
    println(2731189.isPrime)
    println(517935871.isPrime)
    println(0.isPrime)
    println(1.isPrime)
    println((-2003).isPrime)
  }



  def testDecompose() = {
    println(2.decompose)
    println(15.decompose)
    println(2003.decompose)
    println(2731189.decompose)
    println(517935871.decompose)
    println(0.decompose)
    println(1.decompose)
    println((-2003).decompose)
  }

  testDecompose()

}
