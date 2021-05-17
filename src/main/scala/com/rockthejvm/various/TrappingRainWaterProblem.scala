package com.rockthejvm.various

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-05-03
  *         Problem Statement: https://leetcode.com/problems/trapping-rain-water/
  *         Technique: Two pointers
  *
 */
 
 
object TrappingRainWaterProblem {

  def trapWater(height: List[Int]): Int = {

    @tailrec
    def trapWaterTailRec(leftMax: Int, rightMax: Int, result: Int, i: Int, j: Int): Int = {
      if(i > j) result
      else {
        if (leftMax <= rightMax) {
          val current = height(i)
          val newLeftMax = Math.max(current, leftMax)
          trapWaterTailRec(newLeftMax, rightMax, result + (newLeftMax - current), i + 1, j)
        }
        else {
          val current = height(j)
          val newRightMax = Math.max(current, rightMax)
          trapWaterTailRec(leftMax, newRightMax, result + (newRightMax - current), i, j - 1)
        }
      }
    }
    if (height.isEmpty) 0
    else trapWaterTailRec(height.head, height.last, 0, 0, height.size - 1)
  }

  def main(args: Array[String]): Unit = {
    println(trapWater(List(0,1,0,2,1,0,1,3,2,1,2,1)))
    println(trapWater(List(4,2,0,3,2,5)))
  }
}
