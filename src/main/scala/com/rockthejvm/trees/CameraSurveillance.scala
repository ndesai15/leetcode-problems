package com.rockthejvm.trees

import com.rockthejvm.trees

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-04-22
  *         Problem Statement: https://leetcode.com/problems/binary-tree-cameras/
  *
 */
 
 
object CameraSurveillance {
  /*
      Given a binary tree, we install cameras on the nodes of the tree.
      Each camera at a node can monitor its parent, itself, and its immediate children.
      Calculate the minimum number of cameras needed to monitor all nodes of the tree

   */

  /*
       1
      / \
     2   3
    /
   4

   Complexity: O(N) time, O(N) space
   */
  val COVERED = 0
  val NOT_COVERED = 1
  val CAMERA = 2

  def cameraSurveillance[T](tree: BTree[T]): Int = {

    def minCamerasStack(node: BTree[T]): (Int, Int) = {
      if (node.isEmpty) (0, COVERED)
      else {
        val (leftNumCameras, leftState) = minCamerasStack(node.left)
        val (rightNumCameras, rightState) = minCamerasStack(node.right)

        /*
          - left or right is NOT COVERED => place camera in this node
          - left or right HAVE CAMERAS => considered the root COVERED
          - consider the root NOT_COVERED
         */

        if (leftState == NOT_COVERED || rightState == NOT_COVERED) (leftNumCameras + rightNumCameras + 1, CAMERA)
        else if (leftState == CAMERA || rightState == CAMERA) (leftNumCameras + rightNumCameras, COVERED)
        else (leftNumCameras + rightNumCameras, NOT_COVERED)
      }
    }

    /*val (stackNumCameras, stackRootState) = minCamerasStack(tree)
    if (stackRootState == NOT_COVERED) stackNumCameras + 1 // additional camera in the root
    else stackNumCameras // root is covered */

    /*
       mct([1], [], []) =
       mct([2 3 1], [1], []) =
       mct([4 End 2 3 1], [1 2], []) =
       mct([End End 4 End 2 3 1], [1 2 4], []) =
       mct([End End 4 End 2 3 1], [1 2 4], [(0, COVERED)]) =
       mct([4 End 2 3 1], [1 2 4], [(0, COVERED), (0, COVERED)]) =
       mct([End 2 3 1], [1 2 4], [(0, NOT_COVERED)]) =
       mct([2 3 1], [1 2 4], [(0, COVERED), (0, NOT_COVERED)]) =
       mct([3, 1], [1 2 4], [(1, >)]) =
       mct([End, End, 3, 1], [1 2 4], [(1, >)]) =
       mct([End, 3, 1], [1 2 4], [(0, COVERED), (1, >)]) =
       mct([3, 1], [1 2 4], [(0, COVERED), (0, COVERED), (1, >)]) =
       mct([1], [1 2 4], [(0,Not_Covered), (1, >)]) =
       mct([], [1,2,4], [(2, CAMERA)]) =
       (2, CAMERA)
     */
    @tailrec
    def minCamerasTail[T](stack: List[BTree[T]],
                          visited: Set[BTree[T]],
                          coverageStack: List[(Int, Int)]): (Int, Int) = {
      if (stack.isEmpty) coverageStack.head
      else {
        val node = stack.head
        if (node.isEmpty) {
          minCamerasTail(stack.tail, visited, (0, COVERED) :: coverageStack)
        }
        else if (!visited.contains(node)) {
          minCamerasTail(node.left :: node.right :: stack, visited + node, coverageStack)
        }
        else {
          val (leftNumCameras, leftState) = coverageStack.head
          val (rightNumCameras, rightState) = coverageStack.tail.head

          val parentState =
            if (leftState == NOT_COVERED || rightState == NOT_COVERED) (leftNumCameras + rightNumCameras + 1, CAMERA)
            else if (leftState == CAMERA || rightState == CAMERA) (leftNumCameras + rightNumCameras, COVERED)
            else (leftNumCameras + rightNumCameras, NOT_COVERED)

          minCamerasTail(stack.tail, visited, parentState :: coverageStack.tail.tail)
        }
      }
    }

    val (tailNumCameras, tailState) = minCamerasTail[T](List(tree), Set[BTree[T]](), List())
    if (tailState == NOT_COVERED) tailNumCameras + 1
    else tailNumCameras

  }
  def main(args: Array[String]): Unit = {
    val smallTree = BTree(1,
      BTree(2,
        BTree(4),
        BTree()),
      BTree(3))

    println(cameraSurveillance(smallTree)) // 2
  }

}
