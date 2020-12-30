package interviews.datastructure.HashTablesProblems

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
  def findFirstRecurringCharacter(array: Array[Int]): Int = {
    // check input, for invalid input array, return -1
    if(array.length == 0) -1
    // convert array to map
    else {
      var arrayToMap: Map[Int, Boolean] = Map()
      for(i<- 0 until array.length) {
        // check if array element exists in a map or not,
        if(arrayToMap.contains(array(i))) {
          // if exists, return an element
          return array(i)
        }
        else {
          arrayToMap = arrayToMap + (array(i) -> true)
        }
      }
      // else return -1
      return -1
    }




  }

  // test cases
  println(findFirstRecurringCharacter(Array(2,5,1,2,1,8,9)))   // should return 2
  println(findFirstRecurringCharacter(Array(2,5,1,1,2,8,9)))   // should return 1
  println(findFirstRecurringCharacter(Array()))   // should return -1
  println(findFirstRecurringCharacter(Array(2,3,4,5)))   // should return -1
}
