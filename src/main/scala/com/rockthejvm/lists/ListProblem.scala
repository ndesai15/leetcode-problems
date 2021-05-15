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

  // random sample
  def sample(k:Int) : RList[T]

  /**
    * Hard difficulty problems
    *
    */
  // sorting the list in the order defined by the Ordering object
  def insertionSort[S >:T](ordering: Ordering[S]): RList[S]
  // merge sort
  def mergeSort[S >:T](ordering: Ordering[S]): RList[S]
  // quick sort
  def quickSort[S >:T](ordering: Ordering[S]): RList[S]
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

  // random sample
  override def sample(k: Int): RList[Nothing] = RNil

  /**
    * Hard difficulty problems
    *
    */
  // sorting the list in the order defined by the Ordering object
  def insertionSort[S >:Nothing](ordering: Ordering[S]): RList[S] = RNil
  // mergeSort
  def mergeSort[S >:Nothing](ordering: Ordering[S]): RList[S] = RNil
  // quickSort
  def quickSort[S >:Nothing](ordering: Ordering[S]): RList[S] = RNil
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

  override def ++[S >: T](anotherList:  RList[S]): RList[S] = {

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

  // the big 3
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

    /*
       [1,2,3].flatMap(x => [x, 2*x]) = betterFlatMap([1,2,3], [])
         = betterFlatMap([2,3], [[2,1]])
         = betterFlatMap([3], [[4,2]],[2,1]])
         = betterFlatMap([], [[6,3], [4,2]], [2,1]])
         = concatenateAll([[6,3], [4,2]], [2,1]], RNil, RNil)
         = concatenateAll([[4,2], [2,1]], [6,3], RNil)
         = concatenateAll([[4,2], [2,1]], [3], [6])
         = concatenateAll([[4,2], [2,1]], [], [3,6])
         = concatenateAll([[2,1]], [4,2], [3,6])
         = concatenateAll([[2,1]],[2],[4,3,6])
         = concatenateAll([[2,1]],[],[2,4,3,6])
         = concatenateAll([[]],[2,1],[2,4,3,6])
         = concatenateAll([[]],[],[1,2,2,4,3,6])
         = [1,2,2,4,3,6]

       Complexity: O(N + Z) ~ O(Z)

     */
    @tailrec
    def betterFlatMap(remaining: RList[T], accumulator: RList[RList[S]]):RList[S] = {
      if(remaining.isEmpty) concatenateAll(accumulator, RNil, RNil)
      else betterFlatMap(remaining.tail, f(remaining.head).reverse :: accumulator)
    }

    /*
        Complexity: O(Z)
     */
    @tailrec
    def concatenateAll(elements: RList[RList[S]], currentList: RList[S], accumulator: RList[S]): RList[S] = {
      if(elements.isEmpty && currentList.isEmpty) accumulator
      else if(currentList.isEmpty) concatenateAll(elements.tail, elements.head, accumulator)
      else concatenateAll(elements, currentList.tail, currentList.head :: accumulator)
    }

    betterFlatMap(this, RNil)
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

  // run-length encoding
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
       [1,2,3].rotate(3) == [1,2,3]
       [1,2,3].rotate(6) == [1,2,3]
       [1,2,3].rotate(4) == [2,3,1]

       [1,2,3].rotate(1) = rotateTailRec([1,2,3], 0, [])
         = rotateTailRec([2,3], 1, [1])
         = [2,3,1]

       [1,2,3].rotate(3) = rotateTailRec([1,2,3], 0, [])
         = rotateTailRec([2,3], 1, [1])
         = rotateTailRec([3], 2, [2,1])
         = rotateTailRec([], 3, [3,2,1])
         = [1,2,3]

       [1,2,3].rotate(4) = rotateTailRec([1,2,3],0,[])
         = rotateTailRec([2,3],1,[1])
         = rotateTailRec([3],2,[2,1])
         = rotateTailRec([],3,[3,2,1])
         = rotateTailRec([1,2,3],3,[])
         = rotateTailRec([2,3],4,[1])
         = [2,3,1]

       Complexity: O(Max(N,K))

     */

    @tailrec
    def rotateTailRec(remainingList: RList[T], rotationsLeft: Int, buffer: RList[T]): RList[T] = {
      if(remainingList.isEmpty && rotationsLeft == k) this
      else if(remainingList.isEmpty) rotateTailRec(this,rotationsLeft, RNil)
      else if(rotationsLeft == k) remainingList ++ buffer.reverse
      else rotateTailRec(remainingList.tail, rotationsLeft + 1, remainingList.head :: buffer)
    }

    if(k < 0) this
    else rotateTailRec(this, 0, RNil)
  }

  // random sample
  override def sample(k: Int): RList[T] = {

    /*
       [1,2,3,4,5].sample(3) = sampleTailRec(0, [])
         = sampleTailRec(1, [4])
         = sampleTailRec(2, [2,4])
         = sampleTailRec(3, [1,2,4])
         = sampleTailRec(4, [1,1,2,4])
         = [1,1,2,4]

       Complexity = O(N * k)
     */

    @tailrec
    def sampleTailRec(sampleRate: Int,sample: RList[T]): RList[T] = {
      if(sampleRate == k) sample
      else {
        val random = scala.util.Random.nextInt(this.length)
        sampleTailRec(sampleRate + 1, this(random) :: sample )
      }
    }

    def sampleElegant: RList[T] =
      RList.from((1 to k).map(_ => scala.util.Random.nextInt(this.length)).map(index => this(index)))

    if(k < 0) RNil else sampleTailRec(0,RNil)
  }

  /**
    * Hard difficulty problems
    *
    */
  // sorting the list in the order defined by the Ordering object
  override def insertionSort[S >:T](ordering: Ordering[S]): RList[S] = {
    /*
       insertSorted(4, [], [1,2,3,5])
         = insertSorted(4, [1], [2,3,5])
         = insertSorted(4, [2,1], [3,5])
         = insertSorted(4, [3,2,1], [5])
         = [3,2,1].reverse ++ (4 :: [5])
         = [1,2,3,4,5]

       Complexity: O(N)
     */

    @tailrec
    def insertSorted(element: T, before: RList[S], after: RList[S]): RList[S] = {
      if(after.isEmpty || ordering.lteq(element, after.head)) before.reverse ++ (element :: after)
      else insertSorted(element, after.head :: before, after.tail)
    }
    /*
      [3,1,4,2,5].insertionSort = insertionSortTailRec([3,1,4,2,5], [])
        = insertionSortTailRec([1,4,2,5],[3])
        = insertionSortTailRec([4,2,5],[1,3])
        = insertionSortTailRec([2,5],[1,3,4])
        = insertionSortTailRec([5],[1,2,3,4])
        = insertionSortTailRec([],[1,2,3,4,5])
        = [1,2,3,4,5]

      Complexity: O(N^2)
     */

    @tailrec
    def insertionSortTailRec(remaining: RList[T], accumulator: RList[S]) : RList[S] = {
      if(remaining.isEmpty) accumulator
      else insertionSortTailRec(remaining.tail, insertSorted(remaining.head, RNil, accumulator))
    }

    insertionSortTailRec(this, RNil)
  }

  override def mergeSort[S >: T](ordering: Ordering[S]): RList[S] = {

    /*
      merge([1,3],[2,4,5,6,7],[]) =
      merge([3],[2,4,5,6,7],[1]) =
      merge([3],[4,5,6,7],[2,1]) =
      merge([],[4,5,6,7],[3,2,1]) =
      [3,1,2].reverse ++ [4,5,6,7] =
      [1,2,3,4,5,6,7]
     */

    @tailrec
    def merge(listA: RList[S], listB: RList[S], accumulator: RList[S]): RList[S] = {
      if(listA.isEmpty) accumulator.reverse ++ listB
      else if(listB.isEmpty) accumulator.reverse ++ listA
      else if(ordering.lteq(listA.head, listB.head)) merge(listA.tail, listB, listA.head :: accumulator)
      else merge(listA,listB.tail, listB.head :: accumulator)
    }

    /*
        [3,1,2,5,4] => [[3],[1],[2],[5],[4]]

        mst([[3],[1],[2],[5],[4]],[]) =
        = mst([[2],[5],[4]], [[1,3]])
        = mst([[4]],[[2,5],[1,3]])
        = mst([], [[4],[2,5],[1,3]])
        = mst([[4],[2,5],[1,3]],[])
        = mst([[1,3]],[[2,4,5]])
        = mst([],[[1,3],[2,4,5]])
        = mst([[1,3],[2,4,5]],[])
        = mst([],[[1,2,3,4,5]])
        = [1,2,3,4,5]

        Complexity: O(n * log(n))
        complexity(n) = 2 * complexity(n/2) + n
     */

    @tailrec
    def mergeSortTailRec(smallLists: RList[RList[S]], bigLists: RList[RList[S]]): RList[S] = {
      if(smallLists.isEmpty) {
        if(bigLists.isEmpty) RNil
        else if(bigLists.tail.isEmpty) bigLists.head
        else mergeSortTailRec(bigLists, RNil)
      }
      else if (smallLists.tail.isEmpty) {
        if(bigLists.isEmpty) smallLists.head
        else mergeSortTailRec(smallLists.head :: bigLists, RNil)
      }
      else {
        val first = smallLists.head
        val second = smallLists.tail.head
        val merged = merge(first, second, RNil)
        mergeSortTailRec(smallLists.tail.tail, merged :: bigLists)
      }
    }
    mergeSortTailRec(this.map(x => x :: RNil), RNil)
  }
  override def quickSort[S >: T](ordering: Ordering[S]): RList[S] = {

    /*
       [5,6,1,4,2].quickSort = partition([6,1,4,2], 5, [], [])
         = partition([1,4,2], 5, [], [6])
         = partition([4,2], 5, [1], [6])
         = partition([2], 5, [4,1], [6])
         = partition([], 5, [2,4,1], [6])
         =([1,4,2], [6])
     */

    @tailrec
    def partition(list: RList[T], pivot: T, smaller: RList[T], larger: RList[T]): (RList[T], RList[T]) = {
      if(list.isEmpty) (smaller.reverse, larger.reverse)
      else
        if(ordering.lteq(pivot, list.head)) partition(list.tail, pivot, smaller, list.head::larger)
        else partition(list.tail, pivot, list.head :: smaller, larger)
    }

    /*
        [3,1,2,5,4].quickSort

        partition([1,2,5,4],3,[],[]) -> ([1,2],[5,4])
        partition([2],1,[],[]) -> ([],[2])
        partition([4],5,[],[]) -> ([4],[])

        quickSortTailRec([[3,1,2,5,4]], []) =
        quickSortTailRec([[1,2],[3],[5,4]],[]) =
        quickSortTailRec([[],[1],[2],[3],[5,4]],[])
        quickSortTailRec([[1],[2],[3],[5,4]],[])
        quickSortTailRec([[2],[3],[5,4]],[[1]])
        quickSortTailRec([[3],[5,4]],[[2],[1]])
        quickSortTailRec([[5,4]],[[3],[2],[1]])
        quickSortTailRec([[4],[],[5]],[[3],[2],[1]])
        quickSortTailRec([],[[5],[4],[3],[2],[1]])
        = [1,2,3,4,5]

        Complexity: O(N^2) in the worst case(when the list is sorted)
        on average O(N * log(N))
     */

    @tailrec
    def quickSortTailRec(remaining: RList[RList[T]], result: RList[RList[T]]): RList[S] = {
      if(remaining.isEmpty) result.flatMap(smallList => smallList).reverse
      else if(remaining.head.isEmpty) quickSortTailRec(remaining.tail, result)
      else if (remaining.head.tail.isEmpty) quickSortTailRec(remaining.tail, remaining.head :: result)
      else {
        val partitioned = partition(remaining.head.tail, remaining.head.head, RNil, RNil)
        val smallList = partitioned._1
        val largeList = partitioned._2
        quickSortTailRec(smallList :: (remaining.head.head :: RNil) :: largeList :: remaining.tail, result)
      }
    }

    quickSortTailRec(this :: RNil, RNil)
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
  val aSmallList = 1 :: 2 :: 3 :: RNil
  val anotherList = 11 :: 22 :: 33 :: 44 :: 55 :: 66 :: 77 :: RNil
  val emptyList = RNil
  val aLargeList = RList.from(1 to 10000)

  def testEasyProblems() = {
    println(aSmallList)

    // test k-th
    println(aSmallList.apply(1)) // 2
    println(aLargeList.apply(8675)) // 8676

    // test length
    println(aSmallList.length) // 3
    println(emptyList.length) // 0
    println(aLargeList.length) // 10000

    // test reverse
    println(aSmallList.reverse) // [3, 2, 1]
    println(aLargeList.reverse) // [10000, 9999, 9997, ...]

    // test concatenate another list
    println(s"Testing Append operation: ${aSmallList ++ aLargeList} ")

    // test remove element
    println(anotherList.removeAt(3))

    // test map
    println(aLargeList.map(x => x + 1))

    // test flatMap
    val currentTime = System.currentTimeMillis()
    aLargeList.flatMap(x => x :: (2 * x) :: RNil)
    println(s"Time to run flatMap on 10000 elements is ${System.currentTimeMillis() - currentTime}" ) // 5 ms

    // test filter
    println(aLargeList.filter(x => x % 2 == 0))
  }

 /**
   * Medium difficulty problems
   */
  def testMediumProblems() = {
    val mediumList = 1 :: 1 :: 2 :: 3 :: 3 :: 4 :: 5 :: RNil
    println(mediumList.rle)
    println(mediumList.duplicateEach(3))

    // test rotate list
    val plainList = RList.from(1 to 10)
    for{
      i <- 1 to 20
    }println(plainList.rotate(i))

    // test random sample
    println(aLargeList.sample(10))

    // test better flatMap
    val currentTime = System.currentTimeMillis()
    aLargeList.flatMap(x => x :: (2 * x) :: RNil)
    println(s"Time to run flatMap on 10000 elements is ${System.currentTimeMillis() - currentTime}" ) // 6 ms
  }

  /**
    * Hard difficulty problems
    */
  def testHardProblems(): Unit = {
    val unsortedList = 3 :: 2 :: 2 :: 3 :: 5 :: 8 :: 9 :: 0  :: RNil
    val listToSort = aLargeList.sample(10)
    val ordering : Ordering[Int] = Ordering.fromLessThan[Int](_ < _)

    // test insertionSort
    println(unsortedList.insertionSort(ordering))
    println(listToSort.insertionSort(ordering))
    // test merge sort
    println(listToSort.mergeSort(ordering))

    // test merge sort edge case
    //println((3 :: RNil).mergeSort(ordering))

    // test quick sort
    //println(listToSort.quickSort(ordering))
  }

  testHardProblems()
}
