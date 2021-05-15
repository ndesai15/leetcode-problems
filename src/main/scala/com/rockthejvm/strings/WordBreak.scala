package com.rockthejvm.strings

/**
  * @author ndesai on 2021-04-29
  *
 */
 
 
object WordBreak {

  def wordBreak(s: String, wordDict: List[String]): Boolean = {
    val result = (1 to s.length).foldLeft(List(0)) {
      case (acc, i) =>
        if(acc.exists(x => wordDict.contains(s.substring(x, i))))
          i :: acc
        else acc
    }

    result.head == s.length

  }

  def main(args: Array[String]): Unit = {
    println(wordBreak("leetcode", List("leet", "code")))
    println(wordBreak("catsandog", List("cats","dog","sand","and","cat")))
  }
}
