package com.rockthejvm.arrays

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-05-06
  *
 */
 
 
object MedianOfTwoSortedArrays {

  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {

    def sep[T](ns: Array[T], k: Int) = math.min(ns.length - 1, (k - 1) / 2)

    @tailrec
    def findKthMin(ns1: Array[Int], ns2: Array[Int], k: Int): Int = {
      if (ns1.isEmpty) {
        println(s"n2k is ${ns2(k)}")
        ns2(k)
      }
      else if (ns2.isEmpty) {
        println(s"n1k is ${ns1(k)}")
        ns1(k)
      }
      else if (k == 0) math.min(ns1(0), ns2(0))
      else {
        // Find midpointers
        val sep1 = sep(ns1, k)
        val sep2 = sep(ns2, k)

        println(s"Separator#1: ${sep1}")
        println(s"Separator#2: ${sep2}")
        println(s"ns1: ${ns1.toList}")
        println(s"ns2: ${ns2.toList}")
        println("comparator = " + ns1(sep1).compare(ns2(sep2)))

        ns1(sep1) compare ns2(sep2) match {
          case 1 => findKthMin(ns1, ns2.drop(sep2 + 1), k - (sep2 + 1))
          case _ => findKthMin(ns1.drop(sep1 + 1), ns2, k - (sep1 + 1))
        }
      }
    }

    val n = nums1.length + nums2.length
    n % 2 match {
      case 0 => (findKthMin(nums1, nums2, n / 2) + findKthMin(nums1, nums2, n / 2 - 1)) / 2.0
      case 1 => findKthMin(nums1, nums2, n / 2)
    }
  }

  def main(args: Array[String]): Unit = {
    //println(findMedianSortedArrays(Array(1,3), Array(2)))
    println(findMedianSortedArrays(Array(1,2), Array(1,3)))

  }
}
