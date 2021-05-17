package com.rockthejvm.graphs

import com.rockthejvm.graphs.GraphProblems.Graph

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-04-18
  *
 */
 
 
object ConnectedComponentsGraph {

  /*
     numOfConnectedComponents(5, List((0,1),(1,2),(3,4))) should return 2
   */

  // Find connected components of a grah
  def numOfConnectedComponents(totalNodes: Int, edges: List[(Int, Int)]): Int = {

    var expanding: Set[Int] = Set()
    val inputGraph: Graph[Int] = (0 until totalNodes).map(num => (num, Set[Int]())).toMap ++
      edges.foldLeft(Map[Int, Set[Int]]()) {
        case (map, (a,b)) => map + (a -> (map.getOrElse(a,Set()) + b))
      }
    println(inputGraph)

    /*
        dfs([0,1,2,3,4], [], (), 0) =
        dfs([1,2,3,4], [0], (), 0) =
        dfs([1,2,3,4], [1,0], (), 0) , expanding(0) =
        dfs([1,2,3,4],[2,1,0], (), 0), expanding(1,0) =
          [0,1,2,3,4] - [1,0] = [2,3,4]
     */

    def dfsTailRec(remaining: Set[Int],
                   stack: List[Int],
                   expanded: Set[Int],
                   counter: Int): Int = {
      -1
    }
    dfsTailRec(inputGraph.keySet, List(), Set(), 0)
  }

  def main(args: Array[String]): Unit = {
    println(numOfConnectedComponents(5, List((0,1),(1,2),(3,4))))
    //println(numOfConnectedComponents(5, List((0,1),(1,2),(2,3),(3,4))))
    //println(numOfConnectedComponents(2, List((1,0))))
    //println(numOfConnectedComponents(3, List((2,3),(1,2),(1,3))))
  }
}
