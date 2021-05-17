package com.rockthejvm.arrays

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-05-17
  *         Problem Statement: https://leetcode.com/problems/rotate-image/
  *
 */
 
 
object RotateImage extends App {

  def rotate(matrix: Array[Array[Int]]): Unit = {

    def rotateMatrix(l: Int, r: Int, top: Int, bottom: Int) = {
      for{
        i <- 0 until (r - l)
      }  {
        val topLeft = matrix(top)(l + i)
        matrix(top)(l + i) = matrix(bottom - i)(l)
        matrix(bottom -i)(l) = matrix(bottom)(r - i)
        matrix(bottom)(r - i) = matrix(top + i)(r)
        matrix(top + i)(r) = topLeft
      }
    }

    @tailrec
    def rotateTailRec(left: Int, right: Int): Unit = {
      if(left > right) ()
      else {
        rotateMatrix(left, right, left, right)
        rotateTailRec(left - 1, right + 1)
      }
    }

    if(matrix.isEmpty) ()
    else {
      rotateTailRec(0, matrix.length - 1)
    }
  }
}
