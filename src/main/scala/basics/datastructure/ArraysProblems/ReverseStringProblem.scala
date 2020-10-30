package basics.datastructure.ArraysProblems

/**
  * @author ndesai on 2020-10-24
  *
 */


object ReverseStringProblem extends App {

  /*
      Problem Statement: Reverse a string
      Example:
        Input => "Jay Bahuchar Mataji"
        Output => "ijataM rahcuhab yaJ"
   */

  // Solution - 1: By allocating extra space
  def reverseString(input: String) : String = {
    // Step# 1: check the input
    if(input.length< 2) input
    // Step# 2: reverse the string
    else input.toCharArray.reverse.mkString("")
  }

  // Solution - 2: In-place with O(1) extra memory
  def reverseStringInPlace(s: Array[Char]): Unit = {
  }
  //println(reverseString(""))
  val tempArray = Array("hello", "world")
  println(tempArray.mkString(" "))
}
