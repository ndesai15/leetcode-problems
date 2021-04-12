package com.rockthejvm.strings

/**
  * @author ndesai on 2021-03-30
  * Problem Statement: Given an input string s, reverse the order of the words.
  * Taken From:
 */
 
 
object ReverseWords {

  /*
      "Jay Bahuchar Mataji" => "Mataji Bahuchar Jay"
      "       hello     world  " => "world hello"
   */

  def reverseWords(string: String): String = {
    string.trim.replaceAll(" +", " ").split(" ").reverse.mkString(" ")
  }

  def reverseWords2(string: String): String = {
    string.split(" ").filter(!_.isEmpty).reverse.mkString(" ")
  }

  def main(args: Array[String]): Unit = {
    println("ReverseWords::::::")
    println(reverseWords("     Jay Bahuchar Mataji     "))
    println(reverseWords("   hello    World     "))
    println(reverseWords("a good   example"))

    println("ReverseWords2::::::")
    println(reverseWords2("     Jay Bahuchar Mataji     "))
    println(reverseWords2("   hello    World     "))
    println(reverseWords2("a good   example"))
  }

}
