package interviews.datastructure.HashTablesProblems

import scala.annotation.tailrec

/**
  * @author ndesai on 8/4/21
  *
 */
 
 
class MyHashMap {

  // In order to minimize the potential collisions,
  // it is advisable to use a prime number as the base of modulo, e.g. 2069
  private val SIZE: Int = 2069

  // Initialize HashMap as a Array of LinkedList (Key, Value) Pair
  // Here LinkedList will act as a bucket container to store (Key, Value)
  val map = Array.ofDim[List[(Int, Int)]](SIZE)

  private def hashCode(key: Int) : Int = key % SIZE

  def put(key: Int, value: Int): Unit = {
    val index = hashCode(key)
    val bucket = map(index)

    if (bucket == null) {
      map(index) = List((key, value))
    } // Nothing stored in bucket
    else {
      @tailrec
      def putTailRec(remaining: List[(Int, Int)]): List[(Int, Int)] = {
        if (remaining.isEmpty) bucket :+ (key, value)
        else if (remaining.head._1 == key) {
          val afterRemovingEntry  = deletePair(remaining.head, bucket)
          afterRemovingEntry :+ (key, value)
        }
        else putTailRec(remaining.tail)
      }
      map(index) = putTailRec(bucket)
    } // Collision
  }

  def get(key: Int): Int = {
    val index = hashCode(key)
    val bucket = map(index)
    if(bucket == null) -1
    else {
      @tailrec
      def getTailRec(remaining: List[(Int, Int)]): Int = {
        if (remaining.isEmpty) -1
        else if (remaining.head._1 == key) remaining.head._2
        else getTailRec(remaining.tail)
      }
      getTailRec(bucket)
    }
  }

  def remove(key: Int): Unit = {
    val index = hashCode(key)
    val bucket = map(index)
    if (bucket == null) ()
    else {
      @tailrec
      def removeTailRec(remaining: List[(Int, Int)]): List[(Int, Int)] = {
        if (remaining.isEmpty) bucket
        else if (remaining.head._1 == key) {
          deletePair(remaining.head, remaining) }
        else removeTailRec(remaining.tail)
      }
      map(index) = removeTailRec(bucket)
    }
  }

  def deletePair(tuple: (Int, Int), tuples: List[(Int, Int)]): List[(Int, Int)] = tuples diff List(tuple)
}

object TestMyHashMap extends App {
  val newMap = new MyHashMap()
  newMap.put(1, 1)
  newMap.put(10, 100)
  newMap.put(10,200)
  newMap.put(20,2010)
  newMap.put(30,3000)
  newMap.put(30,4000)
  println(newMap.get(1))
  println(newMap.get(10))
  println(newMap.get(20))
  println(newMap.get(30))
  newMap.remove(10)
  println(newMap.get(10))
  newMap.remove(30)
  println(newMap.get(30))
}


