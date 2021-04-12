package com.rockthejvm.numbers

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-03-27
  *
 */
 
 
object Duplicates {

  // all numbers in the list appear EXACTLY twice, EXCEPT one: find that number
  def duplicates(list: List[Int]): Int = {

    // Complexity: O(N^2) time, O(1) space
    @tailrec
    def naive(remainder: List[Int]): Int = {
      if(remainder.isEmpty) throw new IllegalArgumentException("list doesn't contain unique number")
      else {
        val element = remainder.head
        val elementCount = list.count(_ == element)

        if(elementCount == 1) element
        else naive(remainder.tail)
      }
    }

    // Complexity: O(N), Space: O(N)
    @tailrec
    def naiveWithMemory(remainder: List[Int], occurences: Map[Int, Int] = Map()): Int = {
      if(remainder.isEmpty) occurences.filter(_._2 == 1).head._1
      else {
        val currentNumber = remainder.head
        val currentOccurences = occurences.getOrElse(currentNumber, 0)

        naiveWithMemory(remainder.tail, occurences + (currentNumber -> (currentOccurences + 1)))
      }
    }

    // Complexity: O(N) time, O(N) with some optimization, at most N/2 elements in the set
    @tailrec
    def withLessMemory(remainder: List[Int], memory: Set[Int] = Set()): Int = {
      if(remainder.isEmpty) memory.head
      else if(memory.contains(remainder.head)) withLessMemory(remainder.tail, memory - remainder.head)
      else withLessMemory(remainder.tail, memory + remainder.head)
    }

    // Optimal
    // ^ = XOR
    // 0 ^ 0 = 0
    // 1 ^ 1 = 0
    // If a is an Int, then a ^ a = 0
    // 0 ^ a = a
    // Complexity: O(N) time, Space: O(1)
    val optimal = list.foldLeft(0)(_ ^ _)
    optimal

    naiveWithMemory(list)
  }

  def main(args: Array[String]): Unit = {
    println(duplicates(List(2,2,1)))
    println(duplicates(List(4,1,2,1,2)))
    println(duplicates(List(1)))
    val first1000 = (1 to 1000).toList
    println(duplicates(first1000 ++ List(5236926) ++ first1000))
  }

}
