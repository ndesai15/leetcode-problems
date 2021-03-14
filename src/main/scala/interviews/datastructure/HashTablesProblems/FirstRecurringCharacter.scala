package interviews.datastructure.HashTablesProblems

import scala.annotation.tailrec

/**
  * @author ndesai on 2020-11-09
  * Google Question
  * Problem Statement: For a given array, find a first recurring element from an array
  * For example:
  *   1)
  *     Input:
  *       [2,5,1,2,3,5,1,2,4]
  *     Output: return 2
  *   2)
  *     Input:
  *       [2,1,1,2,3,5,1,2,4]
  *     Output: return 1
  *   3)
  *     Input: [2,3,4,5]
  *     Output: return undefined or -1
  *  Taken From: https://www.udemy.com/course/master-the-coding-interview-data-structures-algorithms/learn/lecture/12314712#questions
  */
 
 
object FirstRecurringCharacter extends App {

  /* Time Complexity : O(n)
     Space complexity : O(n) */
  def findFirstRecurringCharacterFromArray(array: Array[Int]): Int = {

    @tailrec
    def findFirstRecurringCharacterFromArrayTailRec(input: List[Int], visited: Map[Int, _]): Int = {
      if(input.isEmpty) -1
      else if(visited.contains(input.head)) input.head
      else findFirstRecurringCharacterFromArrayTailRec(input.tail, visited + (input.head -> true))
    }

    if(array.length <= 1) -1
    else findFirstRecurringCharacterFromArrayTailRec(array.toList, Map())
  }

  // test cases
  println(findFirstRecurringCharacterFromArray(Array(2,5,1,2,1,8,9)))   // should return 2
  println(findFirstRecurringCharacterFromArray(Array(2,5,1,1,2,8,9)))   // should return 1
  println(findFirstRecurringCharacterFromArray(Array()))   // should return -1
  println(findFirstRecurringCharacterFromArray(Array(2,3,4,5)))   // should return -1
}
