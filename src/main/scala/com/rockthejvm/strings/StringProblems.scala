package com.rockthejvm.strings

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-01-19
  *
 */
 
 
object StringProblems extends App {

  def countCharacters(s: String): Map[Char, Int] = {

    /*
       countCharactersTailRec("Scala", Map()) = ccTR("cala", [S -> 1])
         = ccTR("ala", [c -> 1, S -> 1])
         = ccTR("la", [a -> 1, c -> 1, S -> 1])
         = ccTR("a", [l -> 1, a -> 1, c -> 1, S -> 1])
         = ccTR("", [l -> 1, a -> 2, c -> 1, S -> 1])
         = [l -> 1, a -> 2, c -> 1, S -> 1]
     */
    @tailrec
    def countCharactersTailRec(remaining: String, acc: Map[Char, Int]): Map[Char, Int] = {
      if(remaining.isEmpty) acc
      else if(acc.contains(remaining.head)) {
        val currentChar = remaining.head
        val currentOccurences = acc(currentChar)
        countCharactersTailRec(remaining.tail , acc + (currentChar -> (currentOccurences + 1)))
      }
      else {
        countCharactersTailRec(remaining.tail, acc + (remaining.head -> 1))
      }
    }

    countCharactersTailRec(s, Map())
  }

  def testCountCharacters() = {
    println(countCharacters("JayBahucharMataji"))
    println(countCharacters("Scala"))
    println(countCharacters("I love Scala and functional programming because it's awesome!"))
  }

  def checkAnagrams(sa: String, sb: String): Boolean = countCharacters(sa) == countCharacters(sb)

  def checkAnagrams2(sa: String, sb: String) = sa.sorted == sb.sorted

  def testCheckAnagrams() = {
    println(checkAnagrams("desserts","stressed"))
    println(checkAnagrams("scala","Haskell"))
    println(checkAnagrams2("desserts","stressed"))
    println(checkAnagrams2("scala","Haskell"))
  }

  testCheckAnagrams()
}
