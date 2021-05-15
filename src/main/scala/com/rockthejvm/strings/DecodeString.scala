package com.rockthejvm.strings

/**
  * @author ndesai on 2021-05-04
  *
 */
 
 
object DecodeString {

  def decodeString(s: String): String = {

    def decodeStringTailRec(counts: List[Int], intermediate: List[String],index: Int, result: String): String = {
      if (index == s.length) result
      else {
        val current = s.charAt(index)

        // if current is digit then push to counts stack
        if(Character.isDigit(current)) {
          var count = 0
          var counterTemp = index
          while(Character.isDigit(s.charAt(counterTemp))){
            count = 10 * count + (s.charAt(counterTemp) - '0')
            counterTemp += 1
          }
          decodeStringTailRec(count :: counts, intermediate, counterTemp, result)
        }
        else if(current == '[') {
          decodeStringTailRec(counts, result :: intermediate, index + 1, "")
        }
        else if(current == ']') {
          // build a string
          val currentString  = intermediate.head
          val count = counts.head
          val newString = currentString + result * count
          decodeStringTailRec(counts.tail, intermediate.tail, index + 1, newString)
        }
        else decodeStringTailRec(counts, intermediate, index + 1, result + s.charAt(index))
      }
    }

    decodeStringTailRec(List(), List(), 0, "")
  }

  def main(args: Array[String]): Unit = {
    println(decodeString("3[a]2[bc]"))
    println(decodeString("3[a2[c]]"))
  }
}
