package basics.datastructure.ArraysProblems

/**
  * @author ndesai on 2020-10-24
  * Problem Statement: Reverse a given string
  * For example:
  *   1)
  *     Input:
  *       String: "Jay Bahuchar Mataji"
  *     Output: "ijataM rahcuhab yaJ"
  *
  *  Taken From: https://www.udemy.com/course/master-the-coding-interview-data-structures-algorithms/learn/lecture/12308750#questions
  *
  *  Hint: For String question, always convert it to array of characters first & then work on that array
  */


object ReverseStringProblem extends App {

  /* Brute force/naive => Convert your string to array of characters & loop through it in backward direction
                            (Bad) Time Complexity: O(n)
                            (Good) Space Complexity: O(1)
   */

  def reverseAString1(input: String): String = {
    // Length of null string will throw NullPointerException
    if (input == null | input == "") input
    else {
      if (input.length > 1) {
        var output = ""
        val inputCharArray = input.split("")
        // check this one more
        inputCharArray.foreach {
          element =>
            output = output + element.mkString("")
        }
        output
      }
      else input
    }
  }

  // Test cases
  println(reverseAString1("Jay Bahuchar Mataji"))
  println(reverseAString1("Hello World"))
  println(reverseAString1("h"))
  println(reverseAString1(null))

  /*
      Better readability
      Approach #1
   */
  def reverseAString2(input:String): String = {
    if (input == null || input == "") input
    else {
      if (input.length > 1) {
        val inputCharArray = input.split("")
        val output = inputCharArray.reverse.mkString("")
        output
      }
      else input
    }
  }

  // Test cases
  println(reverseAString2("Jay Bahuchar Mataji"))
  println(reverseAString2("Hello World"))
  println(reverseAString2("h"))
  println(reverseAString2(null))

  /*
      Better readability
      Approach #1
   */
  def reverseAString3(input:String): String = {
    if (input == null || input == "") input
    else {
      if (input.length > 1) {
        input.reverse
      }
      else input
    }
  }

  println(reverseAString3("Jay Bahuchar Mataji"))
  println(reverseAString3("Hello World"))

}
