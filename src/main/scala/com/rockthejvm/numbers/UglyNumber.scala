package com.rockthejvm.numbers

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/**
  * @author ndesai on 2021-03-27
  *
 */
 
 
object UglyNumber {

  // ugly = only the factors 2, 3 and 5
  // 1 is ugly
  // assume positive inputs
  // examples: 6, 25, 100
  // not ugly: 14, 39
  // Problem : Given an integer n, return true if n is an ugly number.
  //           Ugly number is a positive number whose prime factors only include 2, 3, and/or 5.
  // Taken from: https://leetcode.com/problems/ugly-number/

  @tailrec
  def uglyNumber(number: Int) : Boolean = {
    if(number == 1) true
    else if (number % 2 == 0) uglyNumber(number / 2)
    else if (number % 3 == 0) uglyNumber(number / 3)
    else if (number % 5 == 0) uglyNumber(number / 5)
    else false
  }

  def nthUglyNumber(n: Int) : Int = {

    /*
                1, 2, 3, 4, 5, 6, 8
        Queues:
               [10, 12]
               [9, 12, 15, 18]
               [10, 15, 20, 25, 30]

        Complexity: O(N), Space: O(N)
     */
    def min3(a: Int, b: Int, c: Int): Int = {
      if(a <= b)
        if(a <= c) a else c
      else if(b <= c) b else c
    }

    @tailrec
    def nthUglyTailRec(index: Int, q2: Queue[Int], q3: Queue[Int], q5:Queue[Int]): Int = {
      val min = min3(q2.head, q3.head, q5.head)
      if(index == n) min
      else {
        val newQ2 = (if(min == q2.head) q2.tail else q2).enqueue(min * 2)
        val newQ3 = (if(min == q3.head) q3.tail else q3).enqueue(min * 3)
        val newQ5 = (if(min == q5.head) q5.tail else q5).enqueue(min * 5)

        nthUglyTailRec(index + 1, newQ2, newQ3, newQ5)
      }
    }

    if(n == 1) 1
    else nthUglyTailRec(2,Queue(2), Queue(3), Queue(5))
  }

  def main(args: Array[String]): Unit = {
    println(uglyNumber(1))
    println(uglyNumber(2))
    println(uglyNumber(3))
    println(uglyNumber(5))
    println(uglyNumber(12000)) // true
    println(uglyNumber(14)) // false
    println((1 to 100).map(nthUglyNumber).toList)
  }

}
