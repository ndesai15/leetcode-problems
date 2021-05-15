package com.rockthejvm.numbers

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-05-10
  *
 */
 
 
object TwoSumII {

  def twoSum(numbers: List[Int], target: Int): List[Int] = {

    @tailrec
    def getTwoSumIndices(left: Int, right: Int): List[Int] = {
      if(left >= right) List()
      else if(numbers(left) + numbers(right) > target) getTwoSumIndices(left, right - 1)
      else if(numbers(left) + numbers(right) < target) getTwoSumIndices(left + 1, right)
      else List(left + 1, right + 1)
    }

    if (numbers.isEmpty) List()
    else getTwoSumIndices(0, numbers.length - 1)
  }
}
