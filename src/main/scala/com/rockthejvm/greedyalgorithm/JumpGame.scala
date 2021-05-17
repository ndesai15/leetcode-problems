package com.rockthejvm.greedyalgorithm

/**
  * @author ndesai on 2021-05-11
  *         Problem Statement: https://leetcode.com/problems/jump-game/
  *         Technique: Greedy Algorithm
  *
 */
 
 
object JumpGame {

  def canJump(nums: Array[Int]): Boolean = {

    def canJumpTailRec(index:Int, goalPost: Int): Boolean = {
      if(index < 0) goalPost == 0
      else if (index + nums(index) >= goalPost) canJumpTailRec(index - 1, index)
      else canJumpTailRec(index - 1, goalPost)
    }

    if (nums.isEmpty) false
    else {
      canJumpTailRec(nums.length - 1, nums.length - 1)
    }
  }

  def main(args: Array[String]): Unit = {
    println(canJump(Array(2,3,1,1,4))) // true
    println(canJump(Array(3,2,1,0,4))) // false
    println(canJump(Array(2,0,0))) // true
  }
}
