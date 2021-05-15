package com.rockthejvm.numbers

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-04-26
  *
 */
 
 
object RobotBoundedInCircle {

  def isRobotBounded(instructions: String): Boolean = {

    @tailrec
    def isRobotBoundedTailRec(remaining: String, position: (Int, Int), direction: (Int, Int)): Boolean = {
      if(remaining.isEmpty) (position._1, position._2) == (0,0) || direction != (0, 1)
      else {
        val instruction = remaining.head
        if(instruction == 'G') isRobotBoundedTailRec(remaining.tail, (position._1 + direction._1, position._2 + direction._2), direction)
        else if (instruction == 'L') isRobotBoundedTailRec(remaining.tail, position, (-direction._2, direction._1))
        else  isRobotBoundedTailRec(remaining.tail, position, (direction._2, -direction._1))
      }
    }
    if (!instructions.contains('L') && !instructions.contains('R')) false
    isRobotBoundedTailRec(instructions, (0, 0), (0, 1))
  }
}
