package com.rockthejvm.graphs

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable

/**
  * @author ndesai on 2021-05-01
  *
 */
 
 
object WordLadder {

  def isNeighbour(s1: String, s2: String): Boolean = {
    @tailrec
    def isNeighborTailRec(input1: String, input2: String, result: Int): Boolean = {
      if(input1.isEmpty && input2.isEmpty) result == 1
      else if (result > 1) false
      else if (input1.head != input2.head) isNeighborTailRec(input1.tail, input2.tail, result + 1)
      else isNeighborTailRec(input1.tail, input2.tail, result)
    }
    isNeighborTailRec(s1, s2, 0)
  }

  def ladderLength(beginWord: String, endWord:String, wordList: List[String]): Int = {
    // Create a lookup graph
    val neighbourLookup: Map[String, List[String]] = (beginWord :: wordList).map(s => s -> wordList.filter(t => isNeighbour(s, t))).toMap
    println(s"neighborlookup $neighbourLookup")

    /*
       Map(cog -> List(dog, log),
       dot -> List(hot, dog, lot),
       hit -> List(hot),
       dog -> List(dot, log, cog),
       lot -> List(hot, dot, log),
       hot -> List(dot, lot),
       log -> List(dog, lot, cog))

       pltr(Set(), Q(hit), 1)
         Queue(hot)
       pltr((Set(hit), Q(hot), 2)
         current = hot
         remaining = ()
         Queue(dot, lot)
       pltr(Set(hit, hot), Q(dot, lot), 3) =
         current = dot
         remaining = (lot)
         children = (dog, lot)
         Queue(lot, dog, lot)
       pltr(Set(hit,hot,dot), Q(lot, dog, lot), 4) =
         current = lot
         remaining = (dog, lot)
         children = (log)
         Queue = (dog, lot, log)
       pltr(Set((lot, hit, hot, dot), Q(dog,lot,dog,lot,log), 5) =
         current = dog
         remaining = (lot,dog,lot,long)
         children = (log, cog)
         Queue = (lot,dog,lot,long,log, cog)
       pltr(Set(lot, hit, hot, dot, dog), Q(


     */

     def perLevelTailRec(visited: Set[String], finalQueue: Queue[String] = Queue(beginWord), step: Int): Int = {
       if (finalQueue.isEmpty) step
       else {
         val (current, remaining) = finalQueue.dequeue
         val newVisited = visited + current
         val children: List[String] = neighbourLookup(current).filter(!visited.contains(_))
         if(children.isEmpty)
           perLevelTailRec(newVisited, remaining, step + 1)
         else perLevelTailRec(newVisited, remaining ++ children, step + 1)
       }
    }
    perLevelTailRec(Set(), step = 0)

    /*
    // perform bfs
    val visited = mutable.Set.empty[String]
    val queue = mutable.Queue(beginWord)
    var step = 1
    while(queue.nonEmpty) {
      val numLevel = queue.size
      println(s"numlevel is $numLevel")
      for(i <- 0 until numLevel) {
        val current = queue.dequeue()
        println(s"current is $current")
        if(current == endWord) return step
        visited += current
        val children = neighbourLookup(current).filterNot(visited)
        queue ++= children
      }
      step += 1
    }
    return 0*/
  }

  def main(args: Array[String]): Unit = {
    println(ladderLength("hit", "cog", List("hot","dot","dog","lot","log","cog")))
    //println(ladderLength("hit", "cog", List("hot","dot","dog","lot","log")))
  }
}
