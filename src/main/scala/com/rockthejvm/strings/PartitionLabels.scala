package com.rockthejvm.strings

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-04-25
  *         Problem Statement: https://leetcode.com/problems/partition-labels/
 */
 
 
object PartitionLabels {

  def countPartitionLables(input: String): List[Int] = {
    if(input.isEmpty) List()
    else {
      val inputMap = input.zipWithIndex.toMap // Will give you a map of char -> lastOccuranceIndexOfChar
      @tailrec
      def countPartitionTailRec(remaining: String, start: Int, lastMaxOccurance:Int, index: Int, result: List[Int]): List[Int] = {
        if (remaining.isEmpty) result.reverse
        else {
          val label = remaining.head
          val lastIndexOccurance = inputMap.getOrElse(label,0)
          val maxIndex = Math.max(lastMaxOccurance, lastIndexOccurance)
          if (maxIndex == index) {
            // reset start index, append size to result
            countPartitionTailRec(remaining.tail, index + 1, lastMaxOccurance, index + 1, input.substring(start, index + 1).size :: result)
          }
          else {
            countPartitionTailRec(remaining.tail, start, maxIndex, index + 1, result)
          }
        }
      }

      countPartitionTailRec(input, 0, 0, 0, List())
    }
  }

  def main(args: Array[String]): Unit = {
    println(countPartitionLables("ababcbacadefegdehijhklij"))
  }
}
