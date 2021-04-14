package com.rockthejvm.trees

import scala.collection.immutable.Queue

/**
  * @author ndesai on 2021-04-12
  *
 */
 
 
object PathSum {
  /*
           ____1____
          /         \
         2          6
        / \        / \
       3  4       7  8
           \
           5

       tree, 6 => true
       tree, 7 => false
   */
  // Return true if there is a path from root to a leaf, such that the sum of value is target.
  def hasPathSum(tree: BTree[Int], target: Int) : Boolean = {

    /*
       hp([1],[6]) =
       hp([2,6],[5,5]) =
       hp([6,3,4],[5,3,3]) =
       hp([3,4,7,8],[3,3, -1,-1]) =
       true

       Complexity: O(N) time,O(N) memory
     */

    def hasPathSumStack(tree: BTree[Int], target: Int): Boolean = {
      if (tree.isEmpty) target == 0
      else if (tree.isLeaf) target == tree.value
      else if (tree.left.isEmpty) hasPathSumStack(tree.right, target - tree.value)
      else hasPathSumStack(tree.left, target - tree.value)
    }

    def hasPathSumTailRec(nodes: Queue[BTree[Int]],targets: Queue[Int]): Boolean = {
      if (nodes.isEmpty) false
      else {
        val node = nodes.head
        val targetVal = targets.head
        val children = List(node.left, node.right).filter(!_.isEmpty)
        val childrenTargets = children.map(_ => targetVal - node.value)

        if(node.isLeaf && node.value == targetVal) true
        else hasPathSumTailRec(nodes.tail ++ children, targets.tail ++ childrenTargets)
      }
    }
    hasPathSumTailRec(Queue(tree), Queue(target))
  }

  // all the paths from root to leaf such that the sum of values == target
  def findSumPaths(tree: BTree[Int], target: Int): List[List[Int]] = {

    /*

           ____1____
          /         \
         2          6
        / \        / \
       3  4       7  8
           \
           -1

      sp(1,6) = [2,6].flatMap(f) = [[2,3] [2,4,-1]].map(path => 1 :: path) = [[1 2 3] [1 2 4 -1]]
        sp(2, 5) = [3, 4].flatMap(f) == [[3]].map(path => 2 :: path) ++ [[4, -1]].map(path => 2 :: path) = [[2,3] [2,4,-1]]
          sp(3, 3) = [[3]]
          sp(4,3)= [[-1]].map(path => 4 :: path) = [[4, -1]]
            sp(-1,-1) = [[-1]]

        sp(6,5) = []
     */
    def stackPaths(tree: BTree[Int], currentTarget: Int): List[List[Int]] = {
      if(tree.isEmpty) List()
      else if (tree.isLeaf)
        if (tree.value == currentTarget) List(List(tree.value))
        else List()
      else {
        List(tree.left, tree.right).flatMap { childNode =>
          val subPaths = stackPaths(childNode, currentTarget - tree.value)
          subPaths.map(path => tree.value :: path)
        }
      }
    }

    /*
           ____1____
          /         \
         2          6
        / \        / \
       3  4       7  8
           \
           -1

       tailPaths([1], [6], [], (), []) =
       tp([2 6 1], [5 5 6], [1],(1), []) =
       tp([3 4 2 6 1], [3 3 5 5 6], [2,1], (1,2), []) =
       tp([4 2 6 1], [3 5 5 6], [2,1],(1,2), [1,2,3]) =
       tp([-1 4 2 6 1], [-1 3 5 5 6], [4,2,1], (1,2,4), [1,2,3]) =
       tp([4 2 6 1], [3 5 5 6], [4 2 1], (1 2 4), [[1 2 4 -1][1 2 3]]) =
       tp([2 6 1], [5 5 6], [1], (1 2 4), [[1 2 4 -1][1 2 3]]) =
       tp([6 1], [5 6], [2 1], (1 2 4), [[1 2 4 -1][1 2 3]]) =
       tp([7 8 6 1], [-1 -1 5 6], [6 1], (1 2 4 6), [[1 2 4 -1][1 2 3]]) =
       tp([8 6 1], [-1 5 6], [6 1], (1 2 4 6), [[1 2 4 -1][1 2 3]])
       tp([6 1], [5 6], [6 1], (1 2 4 6), [[1 2 4 -1][1 2 3]]) =
       tp([1],[6], [6 1], (1 2 4 6), [[1 2 4 -1][1 2 3]]) =
       tp([], [], [6 1], (1 2 4 6), [[1 2 4 -1][1 2 3]]) =
       [[1 2 4 -1][1 2 3]]
     */
    def tailPaths(nodes: List[BTree[Int]],
                  targets: List[Int],
                  currentPath: List[BTree[Int]],
                  expanded: Set[BTree[Int]],
                  acc: List[List[BTree[Int]]]): List[List[BTree[Int]]] = {
      if(tree.isEmpty) acc
      else {
        val node = nodes.head
        val currentTarget = targets.head
        val children = List(node.left, node.right).filter(!_.isEmpty)
        val childrenTargets = children.map(_ => currentTarget - node.value)

        if(node.isLeaf)
          if(node.value == currentTarget)
            tailPaths(nodes.tail, targets.tail, currentPath, expanded, (node :: currentPath).reverse.map(_.value) :: acc)
          else
            tailPaths(nodes.tail, targets.tail, currentPath, expanded, acc)
        else
          if(expanded.contains(node))
            tailPaths(nodes.tail, targets.tail, currentPath.tail, expanded, acc)
          else
            tailPaths(children ++ nodes, childrenTargets ++ targets, node :: currentPath, expanded + node, acc)
      }
    }

    stackPaths(tree, target)
  }

  def main(args: Array[String]): Unit = {
    val tree = BNode(
      1, BNode(
        2, BNode(3, BEnd, BEnd),
        BNode(4, BEnd, BNode(5, BEnd, BEnd))
      ),
      BNode(6, BNode(7, BEnd, BEnd), BNode(8, BEnd, BEnd))
    )

    println(hasPathSum(tree, 6)) // true
    println(hasPathSum(tree, 7)) // false

    val treeSumPaths = BNode(
      1, BNode(
        2, BNode(3, BEnd, BEnd),
        BNode(4, BEnd, BNode(-1, BEnd, BEnd))
      ),
      BNode(6, BNode(7, BEnd, BEnd), BNode(8, BEnd, BEnd))
    )

    println(findSumPaths(treeSumPaths, 6))
    println(findSumPaths(treeSumPaths, 7))
  }
}
