package interviews.datastructure.ArraysProblems

/**
  * @author ndesai on 2021-03-06
  * Problem Statement: Given an array of integers nums and an integer target,
  *                    return indices of the two numbers such that they add up to target.
  *
  *  Taken From: https://leetcode.com/problems/two-sum/description/
  *
  *  Hint: For String question, always convert it to array of characters first & then work on that array

  */
 
 
object TwoSumProblem extends App {

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    @scala.annotation.tailrec
    def twoSumTailRec(input: List[Int], result: Array[Int], index: Int, visited: Map[Int, Int]): Array[Int] = {
      if(input.isEmpty) result
      else{
        val current = input.head
        val comp = target - current
        if(visited.contains(comp)) {
          result :+ visited(comp) :+ index
        }
        else {
          twoSumTailRec(input.tail, result, index + 1, visited + (current -> index))
        }
      }
    }
    twoSumTailRec(nums.toList, Array(), 0, Map())
  }
  println(twoSum(Array(2,7,11,15),9).toList)
  println(twoSum(Array(3,2,4),6).toList)
  println(twoSum(Array(3,3),6).toList)
}
