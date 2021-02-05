package com.rockthejvm.graphs

import java.awt.Color

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-01-30
  *
 */
 
 
object GraphProblems extends App {

  type Graph[T] = Map[T, Set[T]]

  val socialNetwork: Graph[String] = Map(
    "Alice" -> Set("Bob", "Charlie", "David"),
    "Bob" -> Set(),
    "Charlie" -> Set("David"),
    "David" -> Set("Bob","Mary"),
    "Mary" -> Set("Bob","Charlie")
  )

  /**
    * Easy problems
    */
  // number of nodes this node `node` is associated (adjacent) to
  def outDegree[T](graph: Graph[T], node: T): Int =
    if(graph.contains(node)) graph(node).size
    else 0

  // number of nodes connected to `node`
  def inDegree[T](graph: Graph[T], node: T): Int = graph.values.count(_.contains(node))

  def testOutDegree():Unit = {
    println(outDegree(socialNetwork, "Alice"))
  }

  def testInDegree(): Unit = {
    println(inDegree(socialNetwork, "David"))
  }

  /**
    * Medium Difficulty Problems
    */
  def isPath[T](graph: Graph[T], start: T, end: T): Boolean = {

    /*
       Alice -> Mary

       isPath(graph, Alice, Mary) =
       iPTR([Alice], []) =
       iPTR([Bob, Charlie, David], [Alice]) =
       iPTR([Charlie, David], [Bob, Alice]) =
       iPTR([David, David], [Charlie, Bob, Alice]) =
       iPTR([David, Bob, Mary], [David, Charlie, Bob, Alice]) =
       iPTR([Bob, Mary], [David, Charlie, Bob, Alice]) =
       iPTR([Mary], [David, Charlie, Bob, Alice]) =
       true

       N nodes, E edges
       Complexity: O(E)
     */
    @tailrec
    def isPathTailRec(connections: List[T], visited: Set[T]): Boolean = {
      if(connections.isEmpty) false
      else {
        val node = connections.head
        if (node == end) true
        else if (visited.contains(node)) isPathTailRec(connections.tail, visited)
        else isPathTailRec(connections.tail ++ graph(node), visited + node)
      }
    }

    if(outDegree(graph, start) == 0) false
    else isPathTailRec(List(start), Set())
  }

  def findPath[T](graph: Graph[T], start: T, end: T): List[T] = {

    /*
       Charlie -> Mary

       fPTR([(Charlie, [Charlie]), Set()] =
         neighbors = [David]
         tuples = [(David, [David, Charlie])]
         fPTR([(David, [David, Charlie])], Set(Charlie)]

       fPTR([(David, [David, Charlie])], Set(Charlie)]
         neighbors = [Bob, Mary]
         tuples = [(Bob,[Bob,David,Charlie]),(Mary,[Mary,David,Charlie])]
         fPTR((Bob,[Bob,David,Charlie]),(Mary,[Mary,David,Charlie]), Set(Charlie, David))

       fPTR([(Mary,[Mary,David,Charlie], Set(Charlie, David, Bob))
         [Charlie, David, Mary]
     */
    @tailrec
    def findPathTailRec(remaining: List[(T, List[T])], consideredNodes: Set[T]): List[T] = {
      if(remaining.isEmpty) List()
      else {
        val(node, currentPath) = remaining.head
        if(node == end) currentPath.reverse
        else if(consideredNodes.contains(node)) findPathTailRec(remaining.tail, consideredNodes)
        else {
          val neighbors = graph(node)
          val tuples = neighbors.map(n => (n, n :: currentPath))
          findPathTailRec(remaining.tail ++ tuples, consideredNodes + node)
        }
      }
    }
    findPathTailRec(graph(start).map(n => (n, n :: List(start))).toList, Set(start))
  }

  def findCycle[T](graph: Graph[T], node:T):List[T] = findPath(graph, node, node)

  def testPath(): Unit = {
    println(isPath(socialNetwork, "Alice", "Mary")) // true
    println(isPath(socialNetwork, "Bob", "Alice")) // false
  }

  def testFindPath(): Unit = {
    println(findPath(socialNetwork, "Alice", "Mary"))
    println(findPath(socialNetwork, "Bob", "Alice"))
    println(findPath(socialNetwork, "Mary", "Alice"))
    println(findPath(socialNetwork, "David", "Alice"))
  }

  def testFindCycle(): Unit = {
    // test cycles
    println(findCycle(socialNetwork, "Mary"))
    println(findCycle(socialNetwork, "Alice"))
  }

  // A -> B AND B -> A
  def makeUndirected[T](graph: Graph[T]): Graph[T] = {
    def addEdge(graph: Graph[T], from: T, to: T): Graph[T] = {
      if(!graph.contains(from)) graph + (from -> Set(to))
      else {
        val neighbors = graph(from)
        graph + (from -> (neighbors + to))
      }
    }

    @tailrec
    def makeUndirectedTailRec(remainingNodes: Set[T], result: Graph[T]): Graph[T] = {
      if(remainingNodes.isEmpty) result
      else{
        val node = remainingNodes.head
        val neighbors = graph(node)
        val newGraph = neighbors.foldLeft(result)((intermediateGraph, neighbor) => addEdge(intermediateGraph, neighbor, node))
        makeUndirectedTailRec(remainingNodes.tail, newGraph)
      }
    }

    makeUndirectedTailRec(graph.keySet, graph)
  }

  def testMakeUndirected() = {
    val undirectedGraph = makeUndirected(socialNetwork)
    println(undirectedGraph("Bob"))
    println(undirectedGraph("Alice"))
    println(undirectedGraph("David"))
  }

  /**
    * Hard Problems
    */

  def color[T](graph: Graph[T]): Map[T, Int] = {
    val undirected = makeUndirected(graph)
    /*
        Alice -> ["Bob", "Charlie", "David"],
        Bob -> ["Alice", "David", "Mary"],
        Charlie -> ["David", "Alice"],
        David -> ["Bob","Mary", "Alice", "Charlie"],
        Mary -> ["Bob","Charlie"]

        [David, Alice, Bob, Charlie, Mary]

        colorTailRec([David, Alice, Bob, Charlie, Mary], 0, {}) =
        colorTailRec([Alice, Bob, Charlie, Mary], 1, {David -> 0}) =
        colorTailRec([Bob, Charlie, Mary], 2, {David -> 0, Alice -> 1, Mary -> 1}) =
        colorTailRec([Charlie, Mary], 3, {David -> 0, Alice -> 1, Mary -> 1, Bob ->2, Charlie -> 2}) =
        colorTailRec([Mary], 3, {David -> 0, Alice -> 1, Mary -> 1, Bob ->2, Charlie -> 2}) =
        colorTailRec([], 3, {David -> 0, Alice -> 1, Mary -> 1, Bob ->2, Charlie -> 2}) =
        {David -> 0, Alice -> 1, Mary -> 1, Bob ->2, Charlie -> 2}

     */

    @tailrec
    def colorTailRec(remainingNodes: List[T], currentColor: Int, coloring: Map[T, Int]): Map[T, Int] = {
      if(remainingNodes.isEmpty) coloring
      else {
        val node = remainingNodes.head
        if(coloring.contains(node)) colorTailRec(remainingNodes.tail, currentColor, coloring)
        else {
          val uncoloredNodes = remainingNodes.tail.foldLeft[Set[T]](Set(node)) {
            (nodesToBeColored, n) =>
              val allNeighbors = nodesToBeColored.flatMap(nodeToBeColored => undirected(nodeToBeColored))
              if(coloring.contains(n) || allNeighbors.contains(n)) nodesToBeColored
              else nodesToBeColored + n
          }
          val newColorings = uncoloredNodes.map((_, currentColor)).toMap
          colorTailRec(remainingNodes.tail, currentColor + 1, coloring ++ newColorings)
        }
      }
    }

    val nodesOrdered = graph.keySet.toList.sortWith((a,b) => outDegree(undirected,a) > outDegree(undirected, b))
    colorTailRec(nodesOrdered, 0, Map())
  }

  def testColor(): Unit = {
    println(color(socialNetwork))
  }

  testColor()
}
