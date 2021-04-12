package com.rockthejvm.strings

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-03-30
  *
 */
 
 
object MultiplyStrings extends App {

  // multiply two numbers represented as strings, of arbitrary length
  def multiplyStrings(a: String, b: String): String = {

    // List(3,2,1) , 6 => List(8,3,7)
    def multiplyByDigits(number: List[Int], factor: Int): List[Int] = {

      /*
         [3,2,1] * 6

         [3,2,1], 0, []
         [2,1], 1, [8]
         [1], 1, [3,8]
         [], 0, [7,3,8]
         => [8, 3, 7]
       */

      def multiplyByDigitsTailRec(remainingDigits: List[Int], carry: Int, acc: List[Int]): List[Int] = {
        if(remainingDigits.isEmpty)
          if(carry ==0) acc.reverse
          else (carry :: acc).reverse
        else {
          val newDigit = remainingDigits.head
          val newProduct = newDigit * factor + carry
          multiplyByDigitsTailRec(remainingDigits.tail, newProduct / 10, (newProduct % 10) :: acc)
        }
      }

      multiplyByDigitsTailRec(number, 0, List())
    }

    /*
       att([9,9,9,9,9,9,9],[9,9,9,9]) =
       att([9,9,9,9,9,9],[9,9,9], 1, [8]) =
       att([9,9,9,9,9],[9,9], 1,[9,8]) =
       att([9,9,9,9],[9],1,[9,9,8]) =
       att([9,9,9],[],1,[9,9,9,8]) =
       [8,9,9,9] ++ att([9,9,9], [1], 0, []) =

       att([9,9,9], [1], 0, []) =
       att([9,9], [], 1, [0]) =
       [0] ++ att([9,9], [1], 0, []) =

       att([9,9], [1], 0, []) =
       att([9], [], 1 [0]) =
       [0] ++ att([9], [1], 0, [])

       att([9], [1], 0, []) =
       att([],[],1, [0]) =
       [1,0].reverse = [0,1]

       [8,9,9,9] ++ att([9,9,9], [1], 0, []) =
       [8,9,9,9] ++ [0] ++ [0] ++ [0,1] =
       [8,9,9,9,0,0,0,1]

     */
    def addTwoNumbers(a: List[Int], b: List[Int]): List[Int] = {

      def addTwoTailRec(remainingA: List[Int], remainingB: List[Int], carry: Int = 0, acc: List[Int] = List()): List[Int] = {
        if(remainingA.isEmpty && remainingB.isEmpty)
          if(carry == 0) acc.reverse
          else (carry :: acc).reverse
        else if(remainingA.isEmpty) acc.reverse ++ addTwoTailRec(List(carry), remainingB, 0, List())
        else if(remainingB.isEmpty) acc.reverse ++ addTwoTailRec(remainingA, List(carry))
        else {
          val newSum = remainingA.head + remainingB.head + carry
          val newDigit = newSum % 10
          val newCarry = newSum / 10
          addTwoTailRec(remainingA.tail, remainingB.tail, newCarry, newDigit :: acc)
        }
      }

      if(a.isEmpty) b
      else if (b.isEmpty) a
      else addTwoTailRec(a, b)
    }

    // 123 * 456 -> List(3,2,1) * List(6,5,4)
    def multiplyDigits(a: List[Int], b: List[Int]): List[Int] = {
      b.zipWithIndex // returns List[(element,index)]
        .map {
        case(element, index) => List.fill(index)(0) ++ multiplyByDigits(a, element)
      } // List[List[Int]] - contains all the partial results
        .reduce(addTwoNumbers)
    }

    val digitsA = a.reverse.map(_ - '0').toList
    val digitsB = b.reverse.map(_ - '0').toList
    val digitsResult = multiplyDigits(digitsA, digitsB)

    val result = digitsResult.reverse.mkString("")

    // TODO check leading zeros
    if(result.isEmpty || result.charAt(0) == '0') "0"
    else result
  }

  println(multiplyStrings("123","456"))

}
