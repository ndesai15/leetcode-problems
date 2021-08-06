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
  def first: A
  def last: MyQueue[A]
  def enqueue[B >:A](element: B): MyQueue[B] = new ConsQueue[B](element, this)
  def dequeue():MyQueue[A]
  def peek():A

  def reverse(): MyQueue[A]
  def printElement: String
  // polymorphic call
  override def toString(): String = "[" + printElement + "]"

}

case object EmptyQueue extends MyQueue[Nothing]{
  override def first: Nothing = throw new NoSuchElementException
  override def last: MyQueue[Nothing] = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def dequeue(): MyQueue[Nothing] = throw new NoSuchMethodException("You can't pop from Empty Stack")
  override def peek(): Nothing = throw new NoSuchElementException("No element in Empty Stack")
  override def reverse(): MyQueue[Nothing] = this
  override def printElement: String = ""
}

case class ConsQueue[+A](override val first: A, override val last: MyQueue[A]) extends MyQueue[A] {
  override def isEmpty: Boolean = false
  def printElement: String = {
    if(last.isEmpty) "" + first
    else first + " " + last.printElement
  }

  def reverse(): MyQueue[A] = {
    def reverseTailRec(remaining: MyQueue[A], result: MyQueue[A]): MyQueue[A] = {
      if (remaining.isEmpty) result
      else reverseTailRec(remaining.last, result.enqueue(remaining.first))
    }

    if (this.last.isEmpty) EmptyQueue.enqueue(this.first)
    else reverseTailRec(this, EmptyQueue)
  }


  override def dequeue(): MyQueue[A] = {
    def dequeueTailRec(remaining: MyQueue[A], result: MyQueue[A]): MyQueue[A] = {
      if (remaining.last.isEmpty) result
      else dequeueTailRec(remaining.last, result.enqueue(remaining.first))
    }

    if (last.isEmpty) EmptyQueue
    else dequeueTailRec(this, EmptyQueue)
  }
  override def peek(): A = {
    def peekTailRec(remaining: MyQueue[A]): A = {
      if (remaining.last.isEmpty) remaining.first
      else peekTailRec(remaining.last)
    }

    if (this.last.isEmpty) this.first
    else peekTailRec(this)
  }
}

object MyQueueTest extends App {
  val queue = ConsQueue("Joy", ConsQueue("Matt", EmptyQueue))
  val anotherQueue = queue.enqueue("Pavel").enqueue("Samir")
  println(anotherQueue.peek()) // Matt
  println(anotherQueue.dequeue().dequeue() toString()) // Pavel -> Samir
  println(anotherQueue.dequeue().dequeue().dequeue().dequeue() toString())
}
