package com.rockthejvm.strings

/**
  * @author ndesai on 2021-03-27
  *
  * ransomNote(
  *   "I have your daughter. I want 1000000 dollars, or you'll never see her again.",
  *   "I bought this really nice doll for my daughter. It was 20 dollars on Amazon. She's never been happier.
  *   I often have discounts from my network, so if you want to buy some really cool stuff for your kids, I can send you an invite
  *   if you sign up to my newsletter. It's read by 100000 people, and you'll never need to search for online discounts again.")
  *
  * Problem statement: ransom note
  * Taken from : https://leetcode.com/problems/ransom-note/
  *
 */
 
 
object RansomNote {
  def ransomNote(note: String, magazine: String): Boolean = {
    def buildMap(string: String): Map[Char, Int] =
      string.groupBy(c => c).mapValues(_.length).toMap

    val noteMap = buildMap(note)
    val magazineMap = buildMap(magazine)
    noteMap.keySet.forall(char => noteMap.getOrElse(char, 0) <= magazineMap.getOrElse(char, 0))
  }

  def main(args: Array[String]): Unit = {
    println(
      ransomNote( "I have your daughter. I want 1000000 dollars, or you'll never see her again.",
        "I bought this really nice doll for my daughter. It was 20 dollars on Amazon. She's never been happier. I often have discounts from my network, so if you want to buy some really cool stuff for your kids, I can send you an inviteif you sign up to my newsletter. It's read by 100000 people, and you'll never need to search for online discounts again."
      )
    )

    println(ransomNote("aa","aab"))
    println(ransomNote("a","b"))
  }


}
