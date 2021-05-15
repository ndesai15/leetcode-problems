package com.rockthejvm.twopointers

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-05-10
  *         Problem Statement: https://leetcode.com/problems/container-with-most-water/
  *
 */
 
 
object ContainerWithWater {

  def maxArea(height: List[Int]): Int = {

    @tailrec
    def calculateMaxArea(left: Int, right: Int, result: Int): Int = {
      if(left >= right) result
      else {
        val leftSide = height(left)
        val rightSide = height(right)

        if(leftSide < rightSide) {
          // change left pointer
          calculateMaxArea(left + 1, right, Math.max(result, (right - left) * Math.min(leftSide, rightSide)))
        }
        else {
          // change right pointer
          calculateMaxArea(left, right - 1, Math.max(result, (right - left) * Math.min(leftSide, rightSide)))
        }
      }
    }

    if (height.isEmpty) 0
    else calculateMaxArea(0, height.length - 1, 0)
  }

  def main(args: Array[String]): Unit = {
    println(maxArea(List(1,8,6,2,5,4,8,3,7)))
    println(maxArea(List(1,1)))
    println(maxArea(List(4,3,2,1,4)))
    println(maxArea(List(1,2,1)))
  }
}
