package interviews.datastructure.ArraysProblems

import scala.annotation.tailrec

/**
  * @author ndesai on 2020-10-31
  * Problem Statement: Merge 2 Sorted Arrays
  * For example:
  *   1)
  *     Input:
  *       ([0, 3, 4, 31],[4,6,30])
  *     Output: [0, 3, 4, 4, 6, 30, 31]
  *
  *  Taken From: https://www.udemy.com/course/master-the-coding-interview-data-structures-algorithms/learn/lecture/12309362#questions
  *
 */
 
 
object MergeSortedArrays extends App {

  /* Brute force/naive =>
                         (Bad) Time Complexity:
                         (Good) Space Complexity:
   */

  def mergeShortArray(array1: Array[Int], array2: Array[Int]): Array[Int] = {
    // Check Input
    if(array2.length == 0) array1
    else if(array1.length == 0) array2
    else {  // case where both the arrays are available
      var mergedArray: Array[Int] = Array()

      var array1Item = array1(0)
      var array2Item = array2(0)
      var i: Int = 1
      var j:Int = 1

      while(i <= array1.length || j <= array2.length) {

        // deadlock starts from here
        if (i > array1.length) {
          // push arrray2 remaining items to final merged array
          if(j < array2.length) {
            // add all the remaining elements of array 2
            for(k <- j-1 to array2.length-1) {
              mergedArray = mergedArray :+ array2(k)
              j = j + 1
            }
          }

        } // ends here
        else if (j > array2.length) {
          // push array1 remaining items to final merged array
          if(i <= array1.length) {
            // add all the remaining elements of array 1
            for(k <- i-1 to array1.length-1) {
              mergedArray = mergedArray :+ array1(k)
              i = i + 1
            }
          }

        }
        else {
          if( array1Item < array2Item ) {
            mergedArray = mergedArray :+ array1Item
            if(i < array1.length) {
              array1Item = array1(i)
            }
            i = i + 1
          }
          else {
            mergedArray = mergedArray :+ array2Item
            if(j < array2.length) {
              array2Item = array2(j)
            }
            j = j + 1
          }
        }
      }
      mergedArray
    }
  }

  // TODO
  def mergeSortedArray(array1: Array[Int], array2: Array[Int]): Array[Int] = {

    @tailrec
    def mergeSortedArrayTailRec(first: Array[Int], second: Array[Int], result: Array[Int]): Array[Int] = {
      if(first.isEmpty && second.isEmpty) result
      else {
        if(first.isEmpty) {
          println("First isEmpty")
          second.flatMap(element => result :+ element)
        }
        else if(second.isEmpty) {
          println("Second isEmpty")
          result.foreach(println)
          println("=======")
          first.flatMap(element => result :+ element)
        }
        else {
          val firstElement = first.head
          val secondElement = second.head
          if (firstElement < secondElement) mergeSortedArrayTailRec(first.tail, second, result :+ firstElement)
          else mergeSortedArrayTailRec(first, second.tail, result :+ secondElement)
        }
      }
    }

    if(array1.isEmpty) array2
    else if(array2.isEmpty) array1
    else mergeSortedArrayTailRec(array1, array2, Array())
  }

  def testMergeSortedWhileLoop() = {
    // test cases
    mergeShortArray(Array(0,2,3), Array()).foreach(println)  // should return Array(0,2,3)
    mergeShortArray(Array(), Array(0,2,3)).foreach(println)  // should return Array(0,2,3)
    mergeShortArray(Array(0,3,4,31),Array(4,5,30)).foreach(println) // should return Array(0,3,4,4,5,30,31)
    mergeShortArray(Array(5,6), Array(1)).foreach(println) // should return Array(1,5,6)
  }

  def testMergeSortedArray() = {
    // test cases
    //println(mergeSortedArray(Array(0,2,3), Array()).toList) // should return Array(0,2,3)
    //println(mergeSortedArray(Array(), Array(0,2,3)).toList) // should return Array(0,2,3)
    //println(mergeSortedArray(Array(0,3,4,31),Array(4,5,30)).toList) // should return Array(0,3,4,4,5,30,31)
    println(mergeSortedArray(Array(5,6), Array(1)).toList) // should return Array(1,5,6)
    //println(mergeSortedArray(Array(1), Array(5,6)).toList) // should return Array(1,5,6)
  }

  testMergeSortedArray()


}
