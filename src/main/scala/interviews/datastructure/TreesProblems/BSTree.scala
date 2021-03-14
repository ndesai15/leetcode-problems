import scala.annotation.tailrec

sealed abstract class BTree[+T] {
  def value: T
  def left: BTree[T]
  def right: BTree[T]

  def isEmpty: Boolean
  def isLeaf: Boolean
  def insert[S >: T](element: S, ordering: Ordering[S]): BTree[S]
  def lookup[S >: T](element: S, ordering: Ordering[S]): Boolean


}

case object BEnd extends BTree[Nothing] {
  override def value: Nothing = throw new NoSuchElementException
  override def left: BTree[Nothing] = throw new NoSuchElementException
  override def right: BTree[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true

  override def isLeaf: Boolean = false
  override def insert[S >: Nothing](element: S, ordering: Ordering[S]): BTree[S] = BNode(element, BEnd, BEnd)
  override def lookup[S >: Nothing](element: S, ordering: Ordering[S]): Boolean = false
}

case class BNode[+T](override val value:T, override val left: BTree[T], override val right: BTree[T]) extends BTree[T] {
  /**
    * Easy Problems
    */
  override def isEmpty: Boolean = false
  override def isLeaf: Boolean = left.isEmpty && right.isEmpty

  override def insert[S >: T](element: S, ordering: Ordering[S]): BTree[S] = {

    @tailrec
    def insertTailRec(todo: List[BTree[S]], expanded: Set[BTree[S]], directions: List[String], done: List[BTree[S]]): BTree[S]= {
      if(todo.isEmpty) done.head
      else if(todo.head.isEmpty) insertTailRec(todo.tail,expanded,directions,done)
      else {
        val currentNode = todo.head
        val currentVal = currentNode.value
        if(currentNode.isLeaf) {
          val elementNode = BNode(element, BEnd, BEnd)
          val newNode = if(ordering.lteq(currentVal, element)) BNode(currentVal, BEnd , elementNode) else BNode(currentVal, elementNode, BEnd)
          insertTailRec(todo.tail, expanded, directions, newNode :: done)
        }
        else if(expanded.contains(currentNode)){
          val direction = directions.head
          val newNode = if(direction == "left") BNode(currentVal, done.head, currentNode.right) else BNode(currentVal, currentNode.left,done.head)
          insertTailRec(todo.tail, expanded, directions, newNode :: done) // i think we need to get head element
        }
        else {
          if(ordering.lteq(currentVal, element)) {
            // go to right
            insertTailRec(currentNode.right :: todo, expanded + currentNode, "right" :: directions, done)
          }
          else {
            // go to left
            insertTailRec(currentNode.left :: todo, expanded + currentNode, "left" :: directions, done)
          }
        }
      }
    }
    insertTailRec(List(this), Set(), List(), List())
  }

  override def lookup[S >: T](element: S, ordering: Ordering[S]): Boolean = {

    @tailrec
    def lookUpTailRec(todo: List[BTree[S]]): Boolean = {
      if(todo.isEmpty) false
      else if(todo.head.isEmpty) lookUpTailRec(todo.tail)
      else if(todo.head.isLeaf) if(ordering.equiv(todo.head.value, element)) true else false
      else{
        val currentNode = todo.head
        val currentNodeValue = currentNode.value
        if(ordering.lt(currentNodeValue, element)) lookUpTailRec(currentNode.right :: todo) // go to right
        else if (ordering.gt(currentNodeValue, element))lookUpTailRec(currentNode.left :: todo)
        else true
      }
    }

    lookUpTailRec(List(this))
  }
}

object BSTree extends App {
  val ordering : Ordering[Int] = Ordering.fromLessThan[Int](_ < _)
  val bstTree = BNode(9,
    BNode(5,
      BNode(4, BEnd, BEnd),
      BNode(6, BEnd, BEnd)
    ),
    BNode(15,
      BNode(10, BEnd, BEnd),
      BNode(17, BEnd, BEnd)
    )
  )

  // test insert operation
  println(bstTree.insert(18, ordering))
  println(bstTree.insert(3,ordering))
  val emptyTree = BEnd
  println(emptyTree.insert(5,ordering).insert(4,ordering).insert(3, ordering).insert(2, ordering).insert(1, ordering))

  // test lookup operation
  println(bstTree.lookup(9, ordering))
  println(bstTree.lookup(17, ordering))
  println(bstTree.lookup(19, ordering))
  println(bstTree.lookup(99, ordering))
  println(bstTree.lookup(6, ordering))
  println(bstTree.lookup(7, ordering))
}