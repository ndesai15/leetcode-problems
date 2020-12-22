package com.rockthejvm.lists

import scala.annotation.tailrec

/**
  * @author ndesai on 2020-12-12
  *
 */

sealed abstract class RList[+T]{ //Covariant List Type

  /*
    class Animal
    class Dog extends Animal
    class RList[Dog] extends RList[Animal]
   */

  /**
    *
    * Standard functions
    */
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[S >: T](element: S): RList[S] = new ::(element, this) // right associative

  /**
    * Easy problems
    */
  def apply(index: Int): T
  def length: Int

  // reverse the list
  def reverse:RList[T]

  // concatenate another list to this one
  def ++[S >:T](anotherList: RList[S]): RList[S]

  // remove an element at a given index, return a new list
  def removeAt(index: Int): RList[T]

  // the big 3
  def map[S](f: T => S): RList[S]
  def flatMap[S](f: T => RList[S]): RList[S]
  def filter(f: T => Boolean): RList[T]

  /**
    * Medium difficulty problems
    *
   */
  // run-length encoding (used in data compression)
  def rle:RList[(T,Int)]

  // duplicate each element a number of times in a row
  def duplicateEach(k: Int) : RList[T]

  // rotation by a number of positions to the left
  def rotate(k: Int): RList[T]
}

case object RNil extends RList[Nothing]{
//                             ^^^^^^^ Nothing is a good substitution of all the Empty list types

  /**
    * Standard functions
    */
  override def head: Nothing = throw new NoSuchElementException
  override def tail: RList[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def toString: String = "[]"

  /**
    * Easy problems
    */
  override def apply(index: Int): Nothing = throw new NoSuchElementException
  override def length: Int = 0

  // reverse the empty list
  override def reverse: RList[Nothing] = this

  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList

  override def removeAt(index: Int): RList[Nothing] = RNil

  // the big 3
  override def map[S](f: Nothing => S): RList[S] = RNil

  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil

  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil

  /**
    * Medium difficulty problems
    *
    */
  // run-length encoding
  override def rle: RList[(Nothing, Int)] = RNil

  // duplicate each element a number of times in a row
  override def duplicateEach(k: Int): RList[Nothing] = RNil

  // rotation by a number of positions to the left
  override def rotate(k: Int): RList[Nothing] = RNil
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {

  override def isEmpty: Boolean = false

  /**
    * Easy problems
    */
  override def toString: String = {

    @tailrec
    def toStringTailRec(inputList:RList[T], result: String): String = {
      if(inputList.isEmpty) result
      else if(inputList.tail.isEmpty) s"$result${inputList.head}"
      else toStringTailRec(inputList.tail, s"$result${inputList.head}, ")
    }

    "[" + toStringTailRec(this, "") + "]"
  }

  override def apply(index: Int): T = {

    /*
       [1,2,3,4 5].apply(2) =>
         applyTailRec([1,2,3,4,5], 0)
         applyTailRec([2,3,4,5], 1)
         applyTailRec([3,4,5], 2)
         3

         Complexity of this algorithm
         O(min(N, index)
         Worst case scenario : O(N)
         Best case scenario : O(1)

     */
    @tailrec
    def applyTailRec(inputList: RList[T], currentIndex: Int) : T = {
      if(currentIndex == index) inputList.head
      else applyTailRec(inputList.tail, currentIndex + 1)
    }

    if(index < 0) throw new NoSuchElementException
    else applyTailRec(this, 0)
  }

  override def length: Int = {

    @tailrec
    def lengthTailRec(input: RList[T], result: Int): Int = {
      if(input.isEmpty) result
      else if (input.tail.isEmpty) result + 1
      else lengthTailRec(input.tail, result + 1)
    }

    lengthTailRec(this,0)
  }

  override def reverse: RList[T] = {
    /*
        [1, 2, 3].reverse => reverseTailRec([1, 2, 3], RNil)
         = reverseTailRec([2, 3], [1])
         = reverseTailRec([3], [2, 1])
         = reverseTailRec([], [3, 2, 1])
         = [3, 2, 1]

         Complexity: O(N)
      */
    @tailrec
    def reverseTailRec(inputList: RList[T], result: RList[T]):RList[T] = {
      if(inputList.isEmpty) result
      else reverseTailRec(inputList.tail, inputList.head :: result)
    }

    reverseTailRec(this, RNil)
  }

  override def ++[S >: T](anotherList:  RList[S]): RList[S] = { //new :: (this.head, this.tail ++ anotherList)

    /*
       Complexity = O(N)
     */
    @tailrec
    def appendTailRec(inputList: RList[S], result: RList[S]): RList[S] = {
      if(inputList.isEmpty) result
      else appendTailRec(inputList.tail, inputList.head :: result)
    }

    appendTailRec(this.reverse, anotherList)
  }

  override def removeAt(index: Int): RList[T] = {

    /*
        [1,2,3,4,5].removeAt(2) = removeAtTailRec([1,2,3,4,5], 0,[])
        = removeAtTailRec([2,3,4,5], 1,[1])
        = removeAtTailRec([3,4,5], 2,[2,1])
        = [2,1].reverse ++ [4,5]

        Complexity : O(N)
     */
    @tailrec
    def removeAtTailRec(remaining: RList[T], currentIndex: Int, predecessor: RList[T]): RList[T] = {
      if(currentIndex == index) predecessor.reverse ++  remaining.tail
      else if(remaining.isEmpty) predecessor.reverse
      else removeAtTailRec(remaining.tail, currentIndex + 1, remaining.head :: predecessor)
    }

    if (index < 0) this
    else removeAtTailRec(this, 0, RNil)
  }

  override def map[S](f: T => S): RList[S] = {
    /*
        [1, 2, 3].map(x => x * 10) = mapTailRec([1,2,3], [])
         = mapTailRec([2,3], [10])
         = mapTailRec([3], [20, 10])
         = mapTailRec([], [30, 20, 10])
         = [30, 20, 10].reverse

        Complexity: O(N)
     */

    @tailrec
    def mapTailRec(remainingList: RList[T], result: RList[S]): RList[S] = {
      if(remainingList.isEmpty) result.reverse
      else mapTailRec(remainingList.tail, f(remainingList.head) :: result)
    }

    mapTailRec(this, RNil)
  }

  override def flatMap[S](f: T => RList[S]): RList[S] = {

    /*
       [1,2,3].flatMap(x => [x, 2*x]) = fmTailRec([1,2,3],[])
        = fmTailRec([2,3],[1,2].reverse)
        = fmTailRec([3],[2,4].reverse ++ [1,2].reverse)
        = fmTailRec([], [3,6].reverse ++ [2,4].reverse ++ [1,2].reverse)
        = [6,3,4,2,2,1].reverse
        = [1,2,2,4,3,6]

       Complexity?
       Real Complexity: O(Z^2)
     */
    @tailrec
    def flatMapTailRec(remainingList: RList[T], result: RList[S]): RList[S] = {
      if(remainingList.isEmpty) result.reverse
      else flatMapTailRec(remainingList.tail, f(head).reverse ++ result) // stack recursive manner
    }

    flatMapTailRec(this, RNil)
  }

  override def filter(f: T => Boolean): RList[T] = {

    /*
       [1,2,3,4].filter(x => x%2 == 0) = filterTailRec([1,2,3,4], [])
         = filterTailRec([2,3,4],[])
         = filterTailRec([3,4],[2])
         = filterTailRec([4],[2])
         = filterTailRec([],[4,2])
         = [4,2].reverse

       Complexity: O(N)
     */

    @tailrec
    def filterTailRec(remainingList: RList[T], result: RList[T]) : RList[T] = {
      if(remainingList.isEmpty) result.reverse
      else filterTailRec(remainingList.tail, if(f(remainingList.head)) remainingList.head :: result else result)
    }

    filterTailRec(this, RNil)
  }

  /**
    * Medium difficulty problems
    *
    */
  override def rle: RList[(T, Int)] = {

    /*
       [1,1,2,2,2,3,4].rle = rleTailRec([1,2,2,2,3,4],(1,1),[])
         = rleTailRec([2,2,2,3,4],(1,2),[])
         = rleTailRec([2,2,3,4],(2,1),[(1,2)])
         = rleTailRec([2,3,4],(2,2),[(1,2)])
         = rleTailRec([3,4],(2,3),[(1,2)])
         = rleTailRec([4],(3,1),[(2,3) , (1,2)])
         = rleTailRec([],(4,1),[(3,1), (2,3) , (1,2)])
         = rleTailRec([],(0,0),[(4,1), (3,1), (2,3) , (1,2)]) = [(4,1), (3,1), (2,3) , (1,2)].reverse
         = [(1,2),(2,3), (3,1),(4,1)]

       Complexity: O(N)
     */
    @tailrec
    def rleTailRec(remainingList: RList[T],currentTuple: (T, Int), result: RList[(T, Int)]): RList[(T, Int)] = {
      if(remainingList.isEmpty && currentTuple._2 == 0)  result
      else if (remainingList.isEmpty) currentTuple :: result
      else if(remainingList.head == currentTuple._1) rleTailRec(remainingList.tail, currentTuple =  currentTuple.copy(_2 = currentTuple._2 + 1), result)
      else rleTailRec(remainingList.tail,(remainingList.head,1), currentTuple :: result)
    }

    rleTailRec(this.tail, (this.head,1), RNil).reverse

  }

  // duplicate each element a number of times in a row
  override def duplicateEach(k: Int): RList[T] = {
    /*
       [1,2,3].duplicateEach(3) = duplicateEachTailRec([1,2,3], [],3)
         = duplicateEachTailRec([1,2,3], [1], 2)
         = duplicateEachTailRec([1,2,3], [1,1], 1)
         = duplicateEachTailRec([1,2,3], [1,1,1], 0)
         = duplicateEachTailRec([2,3], [1,1,1], 3)
           .......
         = duplicateEachTailRec([], [3,3,3,2,2,2,1,1,1], 0)
         =  [1,1,1,2,2,2,3,3,3]

       Complexity = O(N)

     */

    @tailrec
    def duplicateEachTailRec(remaining: RList[T], result: RList[T],t:Int): RList[T] = {
      if(remaining.isEmpty) result.reverse
      else if(t ==0) duplicateEachTailRec(remaining.tail, result,k) // pass initial k
      else duplicateEachTailRec(remaining, remaining.head :: result, t -1)
    }

    duplicateEachTailRec(this,RNil, k)
  }

  // rotation by a number of positions to the left
  override def rotate(k: Int): RList[T] = {

    /*
       [1,2,3,4,5].rotate(3) = rotateTailRec([1,2,3,4,5],0,[])
         = rotateTailRec([2,3,4,5],1,[1])
         = rotateTailRec([3,4,5],2,[2,1])
         = rotateTailRec([4,5],3,[3,2,1])
         = [4,5] ++ [1,2,3]
         = [4,5,1,2,3]

       Complexity: O(Min(N,K)

     */

    def rotateTailRec(remainingList: RList[T], currentIndex: Int, predecessor: RList[T]): RList[T] = {
      if(currentIndex == k) remainingList ++ predecessor.reverse
      else if(remainingList.isEmpty) predecessor.reverse
      else rotateTailRec(remainingList.tail, currentIndex + 1, remainingList.head :: predecessor)
    }

    if(k < 0) this
    else rotateTailRec(this, 0, RNil)
  }

}

// create a large list
object RList{

  def from[T](iterable: Iterable[T]): RList[T] = {

    @tailrec
    def fromTailRec(inputIterable: Iterable[T], acc: RList[T]): RList[T] = {
      if(inputIterable.isEmpty) acc
      else fromTailRec(inputIterable.tail, inputIterable.head :: acc)
    }

    fromTailRec(iterable, RNil).reverse

  }
}

object ListProblem extends App {
  def testEasyProblems() = {
    val aSmallList = 1 :: 2 :: 3 :: RNil
    val anotherList = 11 :: 22 :: 33 :: 44 :: 55 :: 66 :: 77 :: RNil
    val emptyList = RNil
    val aLargeLiist = RList.from(1 to 10000)
    println(aSmallList)

    // test k-th
    println(aSmallList.apply(1)) // 2
    println(aLargeLiist.apply(8675))

    // test length
    println(aSmallList.length) // 3
    println(anotherList.length) // 7
    println(emptyList.length) // 0
    println(aLargeLiist.length)

    // test reverse
    println(aSmallList.reverse)
    println(aLargeLiist.reverse)

    // test concatenate another list
    println(aSmallList ++ aLargeLiist)

    // test remove element
    println(anotherList.removeAt(3))

    // test map
    println(aLargeLiist.map(x => x + 1))

    // test flatMap
    val currentTime = System.currentTimeMillis()
    aLargeLiist.flatMap(x => x :: (2 * x) :: RNil)
    println(System.currentTimeMillis() - currentTime)

    // test filter
    println(aLargeLiist.filter(x => x % 2 == 0))
  }

 /**
   * Medium difficulty problems
   */
  def testMediumProblems() = {
    val mediumList = 1 :: 1 :: 2 :: 3 :: 3 :: 4 :: 5 :: RNil
    println(mediumList.rle)
    println(mediumList.duplicateEach(3))

    // test rotate list
    val plainList = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: RNil
    println(plainList.rotate(3))
  }
  testMediumProblems()
}
