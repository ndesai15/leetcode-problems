package com.rockthejvm.numbers

/**
  * @author ndesai on 2021-03-15
  *
  * Problem Statement: Given a list of non-negative integers nums, arrange them such that they form the largest number
  *
  * Note: The result may be very large, so you could return a String instead of an integer
  * For example:
  *   1)
  *     Input:
  *       nums = [10, 2]
  *     Output: "210"
  *   2)
  *     Input:
  *       nums = [3,30,34,5,9]
  *     Output: "9534330"
  *
  * Taken From: https://leetcode.com/problems/largest-number/
 */
 
 
object LargestNumber {

  def largestNumber(numbers: List[Int]): String = {
    // "given" instance in Scala 3
    implicit val newOrdering: Ordering[Int] = Ordering.fromLessThan{ (a, b) =>
      // concatenate a with b => ab
      // concatenate b with a => ba
      // comparison: a comes before b if ab >= ba
      val aString = a.toString
      val bString = b.toString

      (aString + bString).compareTo(bString + aString) >= 0
    }

    /*
     Properties of sorting method
       - reflective: a <= a
       - anti-symmetrical: if a <= b AND b <= a then a == b
         THIS IS NOT THE CASE for proper sorting
         List(1010, 10) counterexample
         for our problem IT DOES NOT MATTER
       - transitive: if a <= b AND b <= c then a <= c
     */

    val largest = numbers.sorted.mkString("")

    if(numbers.isEmpty || largest.charAt(0) == '0') "0"
    else largest
  }

  def main(args: Array[String]): Unit = {
    println(largestNumber(List(10, 2))) // 210
    println(largestNumber(List(3, 30, 5, 9, 34))) // 9534330
    println(largestNumber(List(2020, 20, 1010,10, 2, 22))) // 222202020101010
    println(largestNumber(List(1))) // 1
    println(largestNumber(List())) // 0
    println(largestNumber(List(0, 0, 0))) // 0
  }

}
