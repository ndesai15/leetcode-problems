package com.rockthejvm.numbers

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-01-19
  *
 */

object RecurringDecimals extends App {

  @tailrec
  def findRecurrenceStart(digit: Long, digits: List[Long], rem: Long, reminders: List[Long], currentIndex: Int): Int = {
    if(digits.isEmpty || reminders.isEmpty) -1
    else if(digit == digits.head && rem == reminders.head) currentIndex
    else findRecurrenceStart(digit, digits.tail, rem, reminders.tail, currentIndex + 1)
  }
  def fractionToRecurringDecimals(numerator: Int, denominator: Int): String = {
    /*
       1/3 = fdt(1, 3, [], []) { quot = 3, rem = 1 }
           = fdt(1, 3, [3], [1]) { quot = 3, rem = 1 } <--- starts HERE
           = fdt(1, 3, [3,3], [1,1]) { quot = 3, rem = 1 }
             .... recurring set of decimals

       1/6 = fdt(1, 6, [], []) { quot = 1, rem = 4 }
           = fdt(4, 6, [1], [4]) { quot = 6, rem = 4 }
           = fdt(4, 6, [1,6], [4,4] { quot = 6, rem = 4 } <----- recurring decimals start here
                          ^
       1/333 = fdt(1, 333, [], []) { quot = 0, rem = 10 }
             = fdt(10, 333, [0], [10]) { quot = 0, rem = 100}
             = fdt(100, 333, [0,0], [10, 100]) { quot = 3, rem = 1 }
             = fdt(1, 333, [0,0,3], [10, 100, 1]) { quot = 0, rem = 10 } <--------- recurring decimals start here

     */

    def f2d(n: Long, d: Long): String = {

      @tailrec
      def fractionDecimalsTailRec(num: Long, denom: Long, digits: List[Long], reminders: List[Long]): String = {
        val quot = (num * 10) / denom
        val rem = (num * 10) % denom

        if(rem == 0) (digits :+ quot).mkString("")
        else {
          val recurrenceIndex = findRecurrenceStart(quot, digits, rem, reminders,0)
          if(recurrenceIndex == -1) fractionDecimalsTailRec(rem, denom, digits :+ quot, reminders :+ rem)
          else {
            val (beforeRecurrence, recurrence) = digits.splitAt(recurrenceIndex)
            s"${beforeRecurrence.mkString("")}(${recurrence.mkString("")})"
          }
        }
      }

      if(n > 0 && d < 0) s"-${f2d(n, -d)}"
      else if(n <  0 && d > 0) s"-${f2d(-n, d)}"
      else {
        val quotient = n / d
        val reminder = n % d

        if(reminder == 0) s"${quotient}"
        else s"${quotient}.${fractionDecimalsTailRec(reminder, d, List(), List())}"
      }
    }

    f2d(numerator, denominator)

    /*
       1/3 = 0.(3)
       1/2 = 0.5
       4/2 = 2
       1/6 = 0.1(6)
       1/333 = 0.(003)
       1/7 = 0.(....)
       1/2003 = 0.(very large set of recurring decimals)

       -1/2
       1/Int.MinValue

       -not 1/Int.MaxValue
     */
  }

  println(fractionToRecurringDecimals(1,3))
  println(fractionToRecurringDecimals(1,2))
  println(fractionToRecurringDecimals(4,2))
  println(fractionToRecurringDecimals(1,6))
  println(fractionToRecurringDecimals(1,333))
  println(fractionToRecurringDecimals(1,7))
  println(fractionToRecurringDecimals(1,2003))
  println(fractionToRecurringDecimals(-1,2))
  println(fractionToRecurringDecimals(1,Int.MinValue))
}
