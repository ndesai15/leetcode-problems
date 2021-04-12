package com.rockthejvm.strings

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-03-27
  *
  * Problem Statement: Given two version numbers, version1 and version2, compare them.
  *
  * Taken From: https://leetcode.com/problems/compare-version-numbers/
 */
 
 
object CompareVersionNumbers {

  /*
     Example: 0.9 < 1.0.3.4 < 1.1.0 < 2.0 < 2.1 == 2.01

     1.0 .... 1.0.0.0
     -1: version1 < version2
     0: version1 == version2
     1: version1 > version2

     Complexity: O(max(L1, L2)) time, space: O(L1 + L2)
   */

  def compareVersionNumbers(version1: String, version2: String): Int = {

    def compare(a: Int, b: Int): Int = {
      if (a > b) 1
      else if (a< b) -1
      else 0
    }

    @tailrec
    def compareVersions(ver1: List[Int], ver2: List[Int]): Int ={
      if(ver1.isEmpty && ver2.isEmpty) 0
      else if(ver1.isEmpty) {
        if(ver2.exists(_ !=0)) -1
        else 0
      }
      else if(ver2.isEmpty) {
        if(ver1.exists(_  !=0)) 1
        else 0
      }
      else {
        val element1 = ver1.head
        val element2 = ver2.head
        val comparedVal = compare(element1, element2)
        if(comparedVal == 0) compareVersions(ver1.tail, ver2.tail)
        else comparedVal
      }
    }

    if(version1.isEmpty || version2.isEmpty) 1
    else {
      val chars1 = version1.split('.').toList
      val chars2 = version2.split('.').toList
      val version1Int: List[Int] = chars1.map(char => char.toInt).toList
      val version2Int: List[Int] = chars2.map(char => char.toInt).toList
      compareVersions(version1Int, version2Int)
    }
  }

  def main(args: Array[String]): Unit = {
    println(compareVersionNumbers("","")) // 1
    println(compareVersionNumbers("0.9","1.0.3.4")) // -1
    println(compareVersionNumbers("1.0.3.4","1.1.0")) // -1
    println(compareVersionNumbers("2.1","2.01")) // 0
    println(compareVersionNumbers("3.1","2.0")) // 1
  }

}
