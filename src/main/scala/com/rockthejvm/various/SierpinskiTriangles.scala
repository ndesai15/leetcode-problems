package com.rockthejvm.various

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-02-21
  *
 */
 
 
object SierpinskiTriangles extends App {

  /*
     n = 3
            *
          *   *
         * * * *
        *       *
       *  *    *  *
      *    *  *    *
     * * * * * * * * *

     n = 2
            *
           * *
          *   *
         * * * *

     n = 1
            *
           * *
   */
  def sierpinski(n: Int): String = {
    def sierpinskiStack(level: Int): List[String] = {
      if(level == 0) List("*")
      else {
        val triangle = sierpinskiStack(level - 1)
        val spaces = " " * (1 << (level - 1)) // 2 ^ (n-1) spaces
        val topTriangle = triangle.map(spaces + _ + spaces)
        val bottomTriangle = triangle.map(row => row + " "+ row)
        topTriangle ++ bottomTriangle
      }
    }

    @tailrec
    def sierpinskiTailRec(currentLevel: Int, currentTriangle: List[String]): List[String] = {
      if(currentLevel >= n) currentTriangle
      else {
        val spaces = "1" * (1 << currentLevel) // 2 ^ (n-1) spaces
        val topTriangle = currentTriangle.map(spaces + _ + spaces)
        val bottomTriangle = currentTriangle.map(row => row + " "+ row)
        sierpinskiTailRec(currentLevel + 1, topTriangle ++ bottomTriangle)
      }
    }

    sierpinskiTailRec(0, List("*")).mkString("\n")
  }

  println(sierpinski(2))
}
