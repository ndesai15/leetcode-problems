package com.rockthejvm.PlayGround

/**
  * @author ndesai on 2020-12-11
  *
 */
 
 
object ScalaRecap extends App {

  val jaybahuchar: Int = 42

  //jaybahuchar = 43

  var mataji = "Jay Bahuchar Mataji"
  // you can reassign var

  // NOTE: Good Scala Code & Pure Functional Programming always use Val not Var

  // everything in scala is Expression, so it's evaluated(reduce) to a value
  val aCondition: Boolean = true
  val someValue = if(aCondition) "Jay Bahuchar Mataji" else "Jay Ambe"

  // code blocks are expressions
  val aCodeBlock = {
    val x = 2 // can define values inside
    x + 99 // value of code block is value of its last expression
  }

  // defining a function
  def aFunction(x: Int) = x + 2

  // functions are recursive
  def factorial(n: Int): Int = {
    if(n <= 1) 1
    else n * factorial(n-1)
  }

  // tail-recursive functions: VERY IMPORTANT FOR THIS COURSE
  def tailRecFactorial(n: Int, accumulator: Int): Int = {
    if(n <= 1) accumulator
    else tailRecFactorial(n-1, n * accumulator)
  }

  /**
    * Object-oriented programming
   */
  class Car
  class SuperCar extends Car

  // subtype polymorphism
  val c: Car = new SuperCar

  // traits are types with abstract (not implemented) methods
  trait SelfDriving {
    def drive: Unit
  }

  // class can extend one class & mix-in any number of traits
  class CarOfFuture extends Car with SelfDriving {
    override def drive: Unit = println("Jay bahuchar Mataji")
  }

  // case-classes are lightweight data structures with boilerplate e.g. equals/hashCode already implemented
  case class Person(name: String, age:Int) {
    def drives(car: Car) = println(s"$name is driving $car")
  }

  // method notation
  val alice = Person("Alice", 23)
  val lamborghini = new SuperCar
  alice drives lamborghini   // <--- infix notation

  /*
   * Functional programming
   */

  // functions are instances of the FunctionX trait
  val function1 = new Function1[Int, Int]{
    override def apply(v1: Int): Int = v1 + 1
  }

  val two = function1(2) // same as incremnter.apply(1)

  // higher-order functions
  val incrementedList = List(1,2,3).map(function1) // function is passed as argument to the map method
  //                                ^^^ this returns a new list

  /*
   * Collections
   */

  // Lists
  val aList: List[Int] = List(1,2,3,4)

  // Sequences = abstract representation of elements in a given order
  val aSeq= Seq(1,2,3,4) // Seq is a trait, so Seq's companion's apply() actually builds a list

  // arrays
  val anArray = Array.ofDim[Int](2,3)   // two dimensional array

  // Sets = every element only appears
  val aSet = Set(2,3,4)

  // Vectors = an efficient Sequence implementation
  val aVector = Vector(1,2,3,4)

  // tuples
  val aTuple:(Int, String) = (1,"Jay Bahuchar Mataji")

  // maps
  val aMap = Map(
    "Jay Bahuchar Mataji" -> 789,
    "Jay Ambe" -> 45
  )






}
