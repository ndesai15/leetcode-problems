package interviews.datastructure.StackQueuesProblems

/**
  * @author ndesai on 2020-11-27
  *
 */
 
 
abstract class MyQueue[+A] {

  /*
     Implement a Stack as a linked list

     FIFO = FIRST IN FIRST OUT

     isEmpty = Stack is Empty or not
     enqueue = push an element to stack
     dequeue = pop first element out of stack
     peek = return first element of stack

   */

  def isEmpty: Boolean
  def head: A
  def tail: MyQueue[A]
  def enqueue[B >:A](element: B): MyQueue[B] = new ConsQueue[B](element, this)
  def dequeue():MyQueue[A]
  def peek():A

  def reverse(): MyQueue[A]
  def printElement: String
  // polymorphic call
  override def toString(): String = "[" + printElement + "]"

}

case object EmptyQueue extends MyQueue[Nothing]{
  override def head: Nothing = throw new NoSuchElementException
  override def tail: MyQueue[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def dequeue(): MyQueue[Nothing] = throw new NoSuchMethodException("You can't pop from Empty Stack")
  override def peek(): Nothing = throw new NoSuchElementException("No element in Empty Stack")
  override def reverse(): MyQueue[Nothing] = this
  override def printElement: String = ""
}

case class ConsQueue[+A](override val head: A, override val tail: MyQueue[A]) extends MyQueue[A] {
  override def isEmpty: Boolean = false
  def printElement: String = {
    if(tail.isEmpty) "" + head
    else head + " " + tail.printElement
  }

  def reverse(): MyQueue[A] = {
    def reverseTailRec(remaining: MyQueue[A], result: MyQueue[A]): MyQueue[A] = {
      if (remaining.isEmpty) result
      else reverseTailRec(remaining.tail, result.enqueue(remaining.head))
    }

    if (this.tail.isEmpty) EmptyQueue.enqueue(this.head)
    else reverseTailRec(this, EmptyQueue)
  }


  override def dequeue(): MyQueue[A] = {
    def dequeueTailRec(remaining: MyQueue[A], result: MyQueue[A]): MyQueue[A] = {
      if (remaining.tail.isEmpty) result
      else dequeueTailRec(remaining.tail, result.enqueue(remaining.head))
    }

    if (tail.isEmpty) EmptyQueue
    else dequeueTailRec(this, EmptyQueue)
  }
  override def peek(): A = {
    def peekTailRec(remaining: MyQueue[A]): A = {
      if (remaining.tail.isEmpty) remaining.head
      else peekTailRec(remaining.tail)
    }

    if (this.tail.isEmpty) this.head
    else peekTailRec(this)
  }
}

object MyQueueTest extends App {
  val queue = ConsQueue("Joy", ConsQueue("Matt", EmptyQueue))
  val anotherQueue = queue.enqueue("Pavel").enqueue("Samir")
  println(anotherQueue.peek())
  println(anotherQueue.dequeue().dequeue() toString())
  println(anotherQueue.dequeue().dequeue().dequeue().dequeue() toString())

}
