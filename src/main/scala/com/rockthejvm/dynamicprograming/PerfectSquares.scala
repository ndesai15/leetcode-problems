package com.rockthejvm.dynamicprograming
import scala.collection.immutable

/**
  * @author ndesai on 2021-05-07
  *         Problem Statement: https://leetcode.com/problems/perfect-squares/
  *
 */
 
 
object PerfectSquares {

  /*
     Complexity: O(n * n ^ (1/2))
   */

  def numSquares(n: Int): Int = {
    val squares = for {
      i <- 1 to n
      if (i * i <= n)
    } yield (i*i)

    val dp = Array.fill(n+1)(n+1)

    dp(0) = 0

    for {
      i <- 1 until dp.length
      square <- squares
      if(square<=i)
    } yield dp(i) = Math.min(dp(i), dp(i-square) + 1)


    dp(n)
  }

  def main(args: Array[String]): Unit = {
    println(numSquares(12))
    println(numSquares(13))
  }
}
