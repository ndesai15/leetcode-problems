package com.rockthejvm.numbers

/**
  * @author ndesai on 2021-04-26
  *         Problem Statement: https://leetcode.com/problems/k-closest-points-to-origin/
  *
 */
 
 
object KClosestPoints {

  /*
    Complexity: O(n log(n)) as sortBy method used quick sort under the hood
   */
  def kClosest(points: Array[Array[Int]], k: Int): Array[Array[Int]] =
    points.map {
      point => (point, Math.sqrt(Math.pow(point(0) - 0, 2) + Math.pow(point(1) - 0, 2)))
    }.sortBy(_._2).map(_._1).take(k)

  def main(args: Array[String]): Unit = {
    println(kClosest(Array(Array(1, 3),Array(-2, 2)), 1).toList.map(_.toList))
    println(kClosest(Array(Array(3, 3), Array(5, -1), Array(-2, 4)), 2).toList.map(_.toList))
  }
}
