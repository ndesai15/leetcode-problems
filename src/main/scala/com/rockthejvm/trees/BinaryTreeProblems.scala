package com.rockthejvm.trees

import scala.annotation.tailrec

/**
  * @author ndesai on 2021-01-25
  *
 */

sealed abstract class BTree[+T] {
  def value: T
  def left: BTree[T]
  def right: BTree[T]
  def isEmpty: Boolean

  /**
    * Easy Problems
    */
  def isLeaf: Boolean
  def collectLeaves: List[BTree[T]]
  def leafCount: Int

  /**
    * Medium difficulty Problems
    */
  // the number of nodes in the tree
  def size: Int

  // nodes at a given level
  def collectNodes(level: Int): List[BTree[T]]

  // mirror a tree
  def mirror: BTree[T]
}

case object BEnd extends BTree[Nothing] {
  override def value: Nothing = throw new NoSuchElementException
  override def left: BTree[Nothing] = throw new NoSuchElementException
  override def right: BTree[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  /**
    * Easy Problems
    */
  override def isLeaf: Boolean = false
  override def collectLeaves: List[BTree[Nothing]] = List()
  override def leafCount: Int = 0

  /**
    * Medium difficulty Problems
    */
  // the number of nodes in the tree
  override val size: Int = 0

  // nodes at a given level
  override def collectNodes(level: Int): List[BTree[Nothing]] = List()

  // mirror a tree
  override def mirror: BTree[Nothing] = BEnd
}

case class BNode[+T](override val value:T, override val left: BTree[T], override val right: BTree[T]) extends BTree[T] {
  /**
    * Easy Problems
    */
  override def isEmpty: Boolean = false
  override def isLeaf: Boolean = left.isEmpty && right.isEmpty

  override def collectLeaves: List[BTree[T]] = {

    /*
           ____1___
          /        \
         2          6
        / \        / \
       3  4       7   8
           \
            5

       clt([1],[]) = clt([2,6], [])
         = clt([3,4,6],[])
         = clt([4,6], [3])
         = clt([5,6],[3])
         = clt([[6],[5,3])
         = clt([7,8],[5,3])
         = clt([8],[7,5,3])
         = clt([],[8,7,5,3])
         = [8,7,5,3]

     */
    @tailrec
    def collectLeavesTailRec(todo: List[BTree[T]], leaves: List[BTree[T]]): List[BTree[T]] = {
      if(todo.isEmpty) leaves
      else if(todo.head.isEmpty) collectLeavesTailRec(todo.tail, leaves)
      else if(todo.head.isLeaf) collectLeavesTailRec(todo.tail, todo.head :: leaves)
      else {
        val node = todo.head
        collectLeavesTailRec(node.left :: node.right :: todo.tail, leaves)
      }
    }
    collectLeavesTailRec(List(this), List())
  }

  override def leafCount: Int = collectLeaves.length

  /**
    * Medium difficulty problems
    */
  // the number of nodes in the tree
  override val size: Int = 1 + left.size + right.size

  // nodes at a given level
  override def collectNodes(level: Int): List[BTree[T]] = {
    /*
           ____1___
          /        \
         2          6
        / \        / \
       3  4       7   8
           \
            5

          level = 2

          cnTR(0, [{1}])
        = cnTR(1, [{2},{6}]
        = cnTR(2, [{3},{4},{7},{8}])
        = [{3},{4},{7},{8}]
     */

    @tailrec
    def collectNodesTailRec(currentLevel: Int, currentNodes: List[BTree[T]]): List[BTree[T]] = {
      if(currentNodes.isEmpty) List()
      else if(currentLevel == level) currentNodes
      else {
        val expandedNodes = for {
          currentNode <- currentNodes
          children <- List(currentNode.left, currentNode.right) if !children.isEmpty
        } yield children

        collectNodesTailRec(currentLevel + 1, expandedNodes)
      }
    }

    if(level < 0) List()
    else collectNodesTailRec(0,List(this))
  }

  // mirror / swap a tree
  /*
         1                               1
       /   \                          /    \
      2     6                        6      2
     / \   / \        ->            / \    / \
    3   4  7  8                    8   7  4  3
         \                               /
         5                              5


       mTR([1], Set(), [])
     = mTR([2,6,1], Set(1), [])
     = mTR([3,4,2,6,1], Set(1,2), [])
     = mTR([4,2,6,1], Set(1,2), [3])
     = mTR([End,5,4,2,6,1], Set(1,2,4), [3])
     = mTR([5,4,2,6,1], Set(1,2,4), [End,3])
     = mTR([4,2,6,1], Set(1,2,4), [5, End, 3])
     = mTR([2,6,1], Set(1,2,4), [(4 5 End] 3])
     = mTR([6,1], Set(1,2,4), [2 (4 5 End] 3])
     = mTR([7,8,6,1], Set(1,2,4,6), [2 (4 5 End] 3])
     = mTR([8,6,1], Set(1,2,4,6), [7, 2 (4 5 End] 3])
     = mTR([6,1], Set(1,2,4,6), [8, 7, 2 (4 5 End] 3])
     = mTR([1], Set(1,2,4,6), [(6 8 7), 2 (4 5 End] 3])
     = mTR([], Set(1,2,4,6), [(1 (6 8 7) (2 (4 5 End] 3)]
     = (1 (6 8 7) (2 (4 5 End) 3))
   */
  override def mirror: BTree[T] = {

    @tailrec
    def mirrorTailRec(todo: List[BTree[T]], expanded: Set[BTree[T]], done: List[BTree[T]]): BTree[T] = {
      if(todo.isEmpty) done.head
      else {
        val currentNode = todo.head
        if(currentNode.isLeaf || currentNode.isEmpty) {
            // move to done
            mirrorTailRec(todo.tail, expanded, currentNode :: done)
        }
        else if(!expanded.contains(currentNode)) {
          // expand tree
          mirrorTailRec(currentNode.left :: currentNode.right ::todo, expanded + currentNode , done)
        }
        else{
            // form a tree
            val newLeft = done.head
            val newRight = done.tail.head
            val newNode = BNode(currentNode.value, newLeft, newRight)
            mirrorTailRec(todo.tail, expanded, newNode :: done.drop(2))
          }

      }
    }
    mirrorTailRec(List(this), Set(), List())
  }
}
 
object BinaryTreeProblems extends App {

  val tree = BNode(
    1, BNode(
      2, BNode(3, BEnd, BEnd),
      BNode(4, BEnd, BNode(5, BEnd, BEnd))
    ),
    BNode(6, BNode(7, BEnd, BEnd), BNode(8, BEnd, BEnd))
  )

  val tree2 = BNode(
    1, BNode(
      2, BNode(3, BEnd, BEnd),
      BNode(4, BEnd, BEnd)
    ),
    BNode(6, BNode(7, BEnd, BEnd), BEnd)
  )

  println(tree.collectLeaves.map(_.value))
  println(tree.leafCount)
  println(tree.size)

  /**
    * Medium difficulty problems
    */
  // the number of nodes in the tree
  val degenerate = (1 to 100000).foldLeft[BTree[Int]](BEnd)((tree, number) => BNode(number, tree, BEnd))
  println(degenerate.size)

  // nodes at a given level
  println(tree.collectNodes(0).map(_.value))
  println(tree.collectNodes(2).map(_.value))
  println(tree.collectNodes(3).map(_.value))
  println(tree.collectNodes(42434))
  println(tree.mirror)
}
