package com.rockthejvm.numbers

/**
  * @author ndesai on 2021-03-21
  *
 */
 
 
object ParseInteger {

  /*
    Return a number from the string argument:
     - there may be leading spaces, ignore those
     - read the sign character if present
     - read all the digits until the end of the string or until a non-digit character
     - return the number formed from those digits
     - if the number exceeds the int range, return either Int.MinValue (underflow) or Int.MaxValue (overflow)

     "      +1234 is the number I want" => 1234
   */

  def parseInteger(string: String): Int = {

    def parseInt(in: String) : Int = {
      val string1 = in.trim

      val ignorable ="+-,\t\r\n"
      val trimmedString = string1.dropWhile(c => ignorable.indexOf(c) >=0)

      val splittedString = trimmedString.split(" ").toList

      val filteredString = splittedString.filter(_.charAt(0).isDigit)

      val inputNumberString = filteredString.mkString("").toLong

      if(inputNumberString > Int.MaxValue || inputNumberString < Int.MinValue) -1
      else inputNumberString.toInt
    }

    val DIGITS = "0123456789".toSet
    if(string.isEmpty) 0
    else if(!string.matches(".*\\d.*")) 0
    else if(string.contains('-')) -parseInt(string)
    else parseInt(string)

  }

  def main(args: Array[String]): Unit = {
    println(parseInteger("+1234 is the number I want"))
    println(parseInteger("+84828428482 is the number I am parsing"))
    println(parseInteger("+-+045 is the string"))
    println(parseInteger(""))
    println(parseInteger("-42"))
    println(parseInteger("meaning of life is everything"))
  }

}
