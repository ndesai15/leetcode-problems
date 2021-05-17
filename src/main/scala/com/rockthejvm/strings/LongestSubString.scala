package com.rockthejvm.strings

/**
  * @author ndesai on 2021-05-08
  *         Problem Statement: https://leetcode.com/problems/longest-substring-without-repeating-characters/
  *         Technique: Sliding Window
  *
 */
 
 
object LongestSubString {

  def lengthOfLongestSubString(s: String): Int = {

    def calculateLengthOfSubString(left: Int, right: Int, visited: Set[Char], maxLength: Int): Int = {
      if (right == s.length) maxLength
      else {
        val currentChar = s(right)
        if(visited(currentChar)) {
          val newVisited = visited - s(left)
          calculateLengthOfSubString(left + 1, right, newVisited, maxLength)
        }
        else calculateLengthOfSubString(left, right + 1, visited + s(right), Math.max(maxLength, right - left + 1))
      }
    }

    if (s.isEmpty) 0
    else calculateLengthOfSubString(0, 0, Set(), 0)
  }

  def main(args: Array[String]): Unit = {
    println(lengthOfLongestSubString("abcabcbb"))  // 3
    println(lengthOfLongestSubString("bbbbb"))  // 1
    println(lengthOfLongestSubString("pwwkew"))  // 3
    println(lengthOfLongestSubString(" "))  // 1
    println(lengthOfLongestSubString("au"))  // 2
    println(lengthOfLongestSubString("aab"))  // 2
    println(lengthOfLongestSubString("dvdf"))  //3
  }
}
