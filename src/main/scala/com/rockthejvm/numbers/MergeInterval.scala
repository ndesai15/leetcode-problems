package com.rockthejvm.numbers

import java.security.KeyStore.TrustedCertificateEntry

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-05-06
  *
 */
 
 
object MergeInterval {

  def merge(intervals: List[(Int, Int)]): List[(Int, Int)] = {

    @tailrec
    def mergeTailRec(remaining: List[(Int, Int)], result: List[(Int,Int)]): List[(Int, Int)] = {
      if(remaining.tail.isEmpty) (remaining.head :: result).reverse
      else {
        val first = remaining.head
        val second = remaining.tail.head
        if (first._2 >= second._1) {
          // overlapping
          if(first._2 >= second._2) {
            val newTuple = (first._1, first._2)
            mergeTailRec(newTuple :: remaining.drop(2), result)
          }
          else {
            val newTuple = (first._1, second._2)
            mergeTailRec(newTuple :: remaining.drop(2), result)
          }
        }
        else mergeTailRec(remaining.tail, first :: result)
      }
    }

    implicit val ordering: Ordering[(Int, Int)] = Ordering.fromLessThan((a, b) => a._1.compareTo(b._1) < 0)

    if(intervals.isEmpty) List()
    else mergeTailRec(intervals.sorted, List())
  }

  def main(args: Array[String]): Unit = {
    println(merge(List((1,3), (2,6), (8,10), (15,18))))
    println(merge(List((1,4),(4,5))))
    println(merge(List((1,4),(2,3))))
  }
}
