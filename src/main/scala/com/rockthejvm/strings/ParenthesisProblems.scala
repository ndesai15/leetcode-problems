package com.rockthejvm.strings

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-01-24
  *
 */
 
 
object ParenthesisProblems extends App {

  /*
    "()" => true
    "()()" => true
    "(())" => true
    ")(" => false
   */
  def hasValidParentheses(string: String):Boolean = {

    /*
       "(())" -> validParensTailRec("(())", 0)
         = vpt("())", 1)
         = vpt("))", 2)
         = vpt(")", 1)
         = vpt("", 0)
         = true

       "())" -> vpt("())", 0)
         = vpt("))", 1)
         = vpt(")", 0)
         = false

       Complexity: O(length(n))
     */
    @tailrec
    def validParensTailRec(remaining: String, openParens: Int): Boolean = {
      if(remaining.isEmpty) openParens == 0
      else if (openParens == 0 && remaining.head == ')') false
      else if (remaining.head == '(') validParensTailRec(remaining.tail, openParens + 1)
      else validParensTailRec(remaining.tail, openParens - 1)
    }

    validParensTailRec(string, 0)
  }

  def testHasValidParentheses() = {
    println(hasValidParentheses("()"))
    println(hasValidParentheses(")("))
    println(hasValidParentheses("()()"))
    println(hasValidParentheses("(())"))
    println(hasValidParentheses("())"))
    println(hasValidParentheses(")()"))
    println(hasValidParentheses("(()()()()(()()(())))"))
  }

  /*
    n = 1 => List("()")
    n = 2 => List("()()","(())")
    n = 3 => List("()()()","()(())","(())()","((()))","(()())")
   */
  def generateAllValidParentheses(n: Int) : List[String] = {
    /*
      () + () = prepend () = ()()
      ( + () + ) = inject () = (())
      () + () = append () =()()
      => [()(), (())]

      ( + () + ) () = ( () ) ()
      ( ) + ( () + ) =() ( () )
      () + () + () = () () ()

      (( + () + )) = (( () ))
      (() + ( )) = (() ())

     */

    @tailrec
    def genParensTailRec(nRemainingParens: Int, currentStrings: Set[String]): Set[String] = {
      if(nRemainingParens == 0) currentStrings
      else {
        val newStrings = for {
          string: String <- currentStrings
          index <- 0 until string.length
        } yield {
          val (before, after) = string.splitAt(index)
          s"$before()$after"
        }
        genParensTailRec(nRemainingParens -1, newStrings)
      }
    }

    assert(n >= 0)

    if(n == 0) List()
    else genParensTailRec(n-1, Set("()")).toList
  }

  def testGenParens() = {
    println(generateAllValidParentheses(1))
    println(generateAllValidParentheses(2))
    println(generateAllValidParentheses(3))
    println(generateAllValidParentheses(10))
  }

  testGenParens()
}
