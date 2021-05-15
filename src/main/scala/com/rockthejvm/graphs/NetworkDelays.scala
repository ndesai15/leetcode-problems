package com.rockthejvm.graphs

import com.rockthejvm.graphs.GraphProblems.Graph

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-04-21
  *
 */
 
 
object NetworkDelays {

  /*
     n = number of nodes
     times = List((a,b,t))
     (a,b,t) = time between node a and node b is t

     What is the time it takes for the signal to go from the source to ALL the other nodes in the network?
   */

  def computeNetworkDelay(n: Int, times: List[(Int, Int, Int)], source: Int): Int = {
    // "adjacency" list/graph
    val graph: Graph[Int] = times.foldLeft(Map[Int, Set[Int]]()) {
      case (map, (a, b, _)) => map + (a -> (map.getOrElse(a, Set())+ b))
    }
    // "adjacency matrix"
    val weights: Map[(Int, Int), Int] = times.map{
      case (a, b, t) => ((a,b), t)
    }.toMap

    /*
        n = 4,
        times = List((1,2,3),(1,3,10),(1,4,10),(2,3,4),(3,4,2))
        graph = {1 -> Set(2,3,4), 2 -> Set(3), 3 -> Set(4), 4-> Set()}
        weights = {(1,2)->3 , (1,3) -> 10, (1,4) -> 10, (2,3) ->4, (3,4)->2}
        dt([1], [], {})
     */
    @tailrec
    def dijkstraTailrec(expanding: Set[Int],
                        visited: Set[Int],
                        costs: Map[Int, Int]): Map[Int, Int] = {
      if (expanding.isEmpty) costs
      else {
        val node = expanding.minBy(costs)

        val neighborCosts: Map[Int, Int] = graph.getOrElse(node, Set()).map{ neighbor =>
          val currentCosts = costs.getOrElse(neighbor, Int.MaxValue)
          val tentativeCost = costs(node)  + weights((node, neighbor))
          val bestCosts = Math.min(tentativeCost, currentCosts)

          (neighbor, bestCosts)
        }.toMap

        val unvisitedNeighbors = graph.keySet.filterNot(visited) // all the nodes that are not visited

        dijkstraTailrec(expanding - node ++ unvisitedNeighbors, visited + node, costs ++ neighborCosts)

      }
    }

    val initialCosts = (1 to n).map(n => (n, Int.MaxValue)).toMap + (source -> 0)
    val latencies = dijkstraTailrec(Set(source), Set(), initialCosts)

    val maxLatency = latencies.values.max

    if(maxLatency == Int.MaxValue) -1
    else maxLatency
  }

  def main(args: Array[String]): Unit = {
    println(computeNetworkDelay(2, List((1,2,1)), 2)) // -1
    println(computeNetworkDelay(2, List((1,2,1)), 1)) // 1
    println(computeNetworkDelay(4, List((2,1,1),(2,3,1),(3,4,1)),2)) // 2
    println(computeNetworkDelay(4, List((1,2,3),(1,3,10),(1,4,10),(2,3,4),(3,4,2)),1)) // 9
  }
}
