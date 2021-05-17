package com.rockthejvm.numbers

/**
  * @author ndesai on 2021-05-10
  *
 */
 
 
object ThreeSum {

  def threeSum(nums: Array[Int]): List[List[Int]] = {
    if (nums.isEmpty) List.empty[List[Int]]
    val numsS = nums.sorted

    println(s"Sorted List: ${numsS.toList}")

    val ans = numsS.indices.foldLeft(List.empty[List[Int]]) {
      case (acc, i) if numsS(i) <= 0 && (i == 0 || (i > 0 && numsS(i) != numsS(i - 1))) =>
        twoSum(numsS, i, acc)
      case (acc, _) => acc
    }
    ans
  }

  def twoSum(nums: Array[Int], from: Int, ans: List[List[Int]]): List[List[Int]] = {

    @scala.annotation.tailrec
    def loop(left: Int, right: Int, result: List[List[Int]]): List[List[Int]] = {
      if(left >= right) result
      else {
        val sum = nums(from) + nums(left) + nums(right)
        if (sum > 0 || (right < nums.length - 1 && nums(right) == nums(right + 1))) {
          println("I'm in right pointer update")
          loop(left, right - 1, result)
        }
        else if (sum < 0 || (left > from + 1 && nums(left) == nums(left - 1))) {
          println("I'm in left pointer update")
          loop(left + 1, right, result)
        }
        else {
          println(s"Now Left is $left right is $right & currentResult is ${nums(from)}, ${nums(left)}, ${nums(right)}")
          loop(left + 1, right , List(nums(from), nums(left), nums(right)) +: result)
        }
      }
    }
    println(s"from is $from")
    loop(from + 1, nums.length - 1, ans)
  }

  def main(args: Array[String]): Unit = {
    println(threeSum(Array(0,0,0,0)))
  }
}
