package com.rockthejvm.numbers

import scala.util.Random

/**
  * @author ndesai on 2021-01-17
  *
 */
 
 
object ApproximatePi extends App {

  val random = new Random(System.currentTimeMillis())

  // compute Pi using Monte-Carlo algorithm
  def approximatePi(nPoints: Int): Double = {

    val nPointsInsideCircle = (1 to nPoints).map { _ =>
      val x = random.nextDouble()
      val y = random.nextDouble()

      x * x + y * y
    }.count(_ < 1.0) // filter(_ < 1.0).length

    (nPointsInsideCircle * 4.0) / nPoints  // approximation of PI
  }

  println(s"Reference: ${Math.PI}")
  println(approximatePi(1000))
  println(approximatePi(10000))
  println(approximatePi(1000000))
  println(approximatePi(10000000))
}
