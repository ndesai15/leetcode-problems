package basics.datastructure.ArraysProblems

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
            for(k <- j-1 until array2.length) {
              mergedArray = mergedArray :+ array2(k)
              j = j + 1
            }
          }

        } // ends here
        else if (j > array2.length) {
          // push array1 remaining items to final merged array
          if(i < array1.length) {
            // add all the remaining elements of array 1
            for(k <- i-1 until array1.length) {
              mergedArray = mergedArray :+ array2(i)
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

  mergeShortArray(Array(1,2,3,4), Array(5,6)).foreach(println)

}
