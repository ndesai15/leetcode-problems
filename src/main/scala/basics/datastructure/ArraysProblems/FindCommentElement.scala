package basics.datastructure.ArraysProblems

/**
  * @author ndesai on 2020-10-30
  * Problem Statement: Given 2 arrays, create a function that let's user know (true/false) whether these two arrays
  *                    contain any common items
  * For example:
  *   1)
  *     Input:
  *       array1 = ['a','b','c']
  *       array2 = ['d','e','f']
  *     Output: false (there is no common element in 2 arrays)
  *   2)
  *     Input:
  *       array1 = ['a','b','c']
  *       array2 = ['x','y','a']
  *     Output: true (there is a common element 'a'
  *  Taken From: https://www.udemy.com/course/master-the-coding-interview-data-structures-algorithms/learn/lecture/12214986#overview
 */

object FindCommentElement extends App {

  /* Brute force/naive => Loop through your both the arrays & find common element
                            (Bad) Time Complexity: O(a * b)
                            (Good) Space Complexity: O(1)
   */
  def hasCommonElement1[T](array1: Array[Char], array2: Array[Char]) : Boolean = {
    var foundMatch = false
    for (i <- 0 until array1.length) {
      for (j <- 0 until array2.length) {
        if(array1(i) == array2(j)) foundMatch = true
      }
    }
    foundMatch
  }

  // test cases
  println("Test case results from brute force approach:")
  println(hasCommonElement1(Array('a','b','c'), Array('h','z','y'))) // return false
  println(hasCommonElement1(Array('a','b','c'), Array('h','a','y'))) // return true
  println(hasCommonElement1(Array('a','b','c'), Array('h'))) // return false
  println(hasCommonElement1(Array('a','b','c'), Array())) // return false
  println(hasCommonElement1(Array('a','b','h'), Array('h'))) // return true
  
  /*
      Better in Time Complexity, bad Space Complexity =>
        (Good)Time Complexity: O(a + b)
        (Bad) Space Complexity: O(a)
   */

  def hasCommonElement2(array1: Array[Char], array2: Array[Char]) : Boolean = {
    // Initialize map
    var foundMatch = false
    var arrayToMap: Map[Char, Boolean] = Map()
    // Time complexity: O(a)
    for(element <- array1) {
      arrayToMap = arrayToMap + (element -> true)
    }
    // Time complexity: O(b)
    for(element <- array2) {
      if(arrayToMap.contains(element)) foundMatch = true
    }
    foundMatch
  }

  println("Test case results from better time complexity:")
  println(hasCommonElement2(Array('a','b','c'), Array('h','z','y'))) // return false
  println(hasCommonElement2(Array('a','b','c'), Array('h','a','y'))) // return true
  println(hasCommonElement2(Array('a','b','c'), Array('h'))) // return false
  println(hasCommonElement2(Array('a','b','c'), Array())) // return false
  println(hasCommonElement2(Array('a','b','h'), Array('h'))) // return true

  /*
      Better readability
   */
  def hasCommonElement3(array1: Array[Char], array2: Array[Char]): Boolean = {
    array1.exists(element => array2.contains(element))
  }

  println("Test case results for better readability:")
  println(hasCommonElement3(Array('a','b','c'), Array('h','z','y'))) // return false
  println(hasCommonElement3(Array('a','b','c'), Array('h','a','y'))) // return true
  println(hasCommonElement3(Array('a','b','c'), Array('h'))) // return false
  println(hasCommonElement3(Array('a','b','c'), Array())) // return false
  println(hasCommonElement3(Array('a','b','h'), Array('h'))) // return true

}
