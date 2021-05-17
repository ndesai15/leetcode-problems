package com.rockthejvm.strings

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-05-08
  *
 */
 
 
object JaybahucharMataji {

  def solution(s: String, t: String): Boolean = {

    def replaceWithQuestionMarkCharacter(input: String, index: Int, accumulator: String): String = {
      if(input.length == index) accumulator
      else {
        val currentCharacter = input(index)
        if(Character.isDigit(currentCharacter)) {
          // Check if next character is also a digit or not
          var count = 0
          var counterTemp = index

          while(counterTemp != input.length && Character.isDigit(input.charAt(counterTemp))){
            count = 10 * count + (input.charAt(counterTemp) - '0')
            counterTemp += 1
          }
          val replacedString = "?" * count

          replaceWithQuestionMarkCharacter(input, counterTemp, accumulator +  replacedString)
        }
        else replaceWithQuestionMarkCharacter(input, index + 1, accumulator + input(index))
      }
    }

    // Get length of string
    @tailrec
    def lengthOfString(input: String, visited: List[Char], index: Int, accumulator: Int):Int = {
      if(input.length == index) {
        if(visited.isEmpty) accumulator
        else visited.length + accumulator
      }
      else {

        val currentCharacter: Char = input(index)
        println(s"currentChara is ${currentCharacter} && index is ${index}")

        if(Character.isDigit(currentCharacter)) {
          // Check if next character is also a digit or not
          var count = 0
          var counterTemp = index
          while(counterTemp != input.length && Character.isDigit(input.charAt(counterTemp))){
            count = 10 * count + (input.charAt(counterTemp) - '0')
            counterTemp += 1
          }

          lengthOfString(input, visited.drop(count.toString.length), index = counterTemp , count + accumulator)
        }
        else lengthOfString(input, visited.drop(1), index + 1, accumulator + 1)
      }
    }

    def checkSourceOfStrings(str1: String, str2: String): Boolean = {
      if(lengthOfString(str1, str1.toCharArray.toList, 0,0) != lengthOfString(str2, str2.toCharArray.toList, 0,0)) false
      else {
        println("here")
        val replacedString1:String = replaceWithQuestionMarkCharacter(str1, 0, "")
        val replacedString2:String = replaceWithQuestionMarkCharacter(str2, 0, "")
        println(s"jay ${replacedString1}")
        println(s"jay ${replacedString2}")
        compareTwo(replacedString1, replacedString2)
      }
    }

    def compareTwo(s1:String, t:String): Boolean = {
      if(s.isEmpty && t.isEmpty) true
      else if(s.isEmpty || t.isEmpty) false
      else {
        if(s.head == '?' && t.head.isValidChar)compareTwo(s.tail, t.tail)
        else if (t.head == '?' && s.head.isValidChar)compareTwo(s.tail, t.tail)
        else if(s.head.equals(t.head)) compareTwo(s.tail, t.tail)
        else false
      }
    }

    if(s.isEmpty && t.isEmpty) true
    else if(s.isEmpty || t.isEmpty) false
    else {
      println(lengthOfString(s,s.toCharArray.toList,0,0 ))
      //Thread.sleep(5000)
      println(lengthOfString(t,t.toCharArray.toList,0,0 ))

      checkSourceOfStrings(s, t)
      //println(lengthOfString(s,s.toCharArray.toList,0,0 ))
      //Thread.sleep(5000)
      //println(lengthOfString(t,t.toCharArray.toList,0,0 ))

      //true
    }

  }

  def main(args: Array[String]): Unit = {
    println(solution("2pL1","A2le"))
    //println(solution("A2Ple","2PLe")) // true
    //println(solution("ba1", "1Ad")) // false

  }

}
