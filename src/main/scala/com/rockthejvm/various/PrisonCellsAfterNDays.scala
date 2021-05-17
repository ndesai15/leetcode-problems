package com.rockthejvm.various

import scala.annotation.tailrec
import scala.collection.immutable.HashSet

/**
  * @author ndesai on 2021-04-28
  *
 */
 
 
object PrisonCellsAfterNDays {

  def prisonAfterNDays(cells: List[Int], n: Int): List[Int] = {

    def remove(element: Int, list: List[Int]): List[Int] = list diff List(element)

    def cycleTailRec(input: List[Int], remainingDay: Int) : List[Int] = {
      if(remainingDay == 0) input
      else {
        val temp2: List[Int] = input.zipWithIndex.map {
          case (_, index) =>
            if (index == 0 || index == input.length - 1) 0
            else {
              if (input(index - 1) == input (index + 1)) 1
              else 0
            }
        }
        cycleTailRec(temp2, remainingDay - 1)
      }
    }

    @tailrec
    def prisonAfterNDaysTailRec(remaining: List[Int], totalDays: Int, day: Int, store: HashSet[String]): List[Int] = {
      if (totalDays == day) remaining
      else {

        val temp: List[Int] = remaining.zipWithIndex.map {
          case (_, index) =>
            if (index == 0 || index == remaining.length - 1) 0
            else {
              if (remaining(index - 1) == remaining (index + 1)) 1
              else 0
            }
        }

        if(!store.contains(temp.toString())) {
          val newSet = store + temp.toString()
          prisonAfterNDaysTailRec(temp, totalDays, day + 1, newSet)
        }
        else {
          println(s"days $day")
          cycleTailRec(temp, totalDays % day)
        }
      }
    }

    prisonAfterNDaysTailRec(cells, n, 0, HashSet())

  }

  def main(args: Array[String]): Unit = {
    println(prisonAfterNDays(List(0,1,0,1,1,0,0,1), 7))
    //println(prisonAfterNDays(List(1,0,0,1,0,0,1,0),1000000000)) // test case is failing
    println(prisonAfterNDays(List(1,1,0,0,0,0,0,1), 8))
    println(prisonAfterNDays(List(1,1,0,0,0,0,0,1),8))
  }
}
