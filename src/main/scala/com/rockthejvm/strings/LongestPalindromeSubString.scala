package com.rockthejvm.strings

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-04-27
  *
 */
object Solution {
  def longestPalindrome(s: String): String = {

    @tailrec
    def maxAt(startIndex: Int, endIndex: Int): String = {
      if (startIndex < 0 || endIndex > s.length - 1 || s.charAt(startIndex) != s.charAt(endIndex))
        s.substring(startIndex + 1, endIndex)
      else
        maxAt(startIndex - 1, endIndex + 1)
    }

    /*
        "babad", length = 5
        longestPalindromeTailRec(0,"") = largestOdd = b, largestEven = ""
        longestPalindromeTailRec(1, "b") = ba, largestEven = ""
        longestPalindromeTailRec(2, "ba") = bab, largestEven = ""
          .....
     */

    @tailrec
    def longestPalindromeTailRec(currentIndex: Int, result: String): String = {
      if (currentIndex == s.length) result
      else {
        println(s"char at ${currentIndex} ${s.charAt(currentIndex)} & current result is ${result}")
        val largestOdd = maxAt(currentIndex - 1, currentIndex + 1)
        val largestEven = maxAt(currentIndex, currentIndex + 1)
        println(s"largetstOdd Value = ${largestOdd}")
        println(s"largetstEven Value = ${largestEven}")

        if (largestOdd.length > largestEven.length && largestOdd.length > result.length)
          longestPalindromeTailRec(currentIndex + 1, largestOdd)
        else if (largestEven.length > largestOdd.length && largestEven.length > result.length)
          longestPalindromeTailRec(currentIndex + 1, largestEven)
        else
          longestPalindromeTailRec(currentIndex + 1, result)
      }
    }

    longestPalindromeTailRec(0, "")
  }

  def main(args: Array[String]): Unit = {
    //println(longestPalindrome("babad"))
    println(longestPalindrome("cbbd"))
    //println(longestPalindrome("a"))
    //println(longestPalindrome("ac"))
  }
}

