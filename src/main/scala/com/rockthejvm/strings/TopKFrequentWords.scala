package com.rockthejvm.strings

/**
  * @author ndesai on 2021-04-28
  *
 */
 
 
object TopKFrequentWords {

  def topKFrequent(words: List[String], k: Int): List[String] = {
    val mapOfWords: Map[String, Int] = words.foldLeft(Map[String, Int]()) {
      (intermediateMap, s) => intermediateMap + (s -> (intermediateMap.getOrElse(s,0) + 1))
    }

    implicit val ordering: Ordering[(String, Int)] = Ordering.fromLessThan{
      (a,b) =>
      if (a._2 == b._2) a._1.compareTo(b._1) < 0
      else a._2.compareTo(b._2) >=0
    }

    val outputList: List[(String, Int)] = mapOfWords.toList.sorted
    outputList.map(_._1).take(k)
  }

  def main(args: Array[String]): Unit = {
    println(topKFrequent(List("i", "love", "leetcode", "i", "love", "coding"), 2))
  }
}
