package basics.datastructure.StackQueuesProblems

/**
  * @author ndesai on 2020-11-27
  *
 */
 
 
abstract class MyQueue[+A] {

  /*
     Implement a Stack as a linked list

     LIFO = LAST IN FIRST OUT

     isEmpty = Stack is Empty or not
     enqueue = push an element to stack
     dequeue = pop first element out of stack
     peek = return first element of stack

   */

  def isEmpty: Boolean
  def enqueue[B >:A](element: B): MyQueue[B]
  def dequeue():MyQueue[A]
  def peek():A

  def printElement: String
  // polymorphic call
  override def toString(): String = "[" + printElement + "]"

}

case object EmptyQueue extends MyQueue[Nothing]{

  override def isEmpty: Boolean = true

  override def enqueue[B >: Nothing](element: B): MyQueue[B] = ConsQueue(element,this)

  override def dequeue(): MyQueue[Nothing] = throw new NoSuchMethodException("You can't pop from Empty Stack")

  override def peek(): Nothing = throw new NoSuchElementException("No element in Empty Stack")

  override def printElement: String = ""

}

case class ConsQueue[+A](head: A, tail: MyQueue[A]) extends MyQueue[A] {

  override def isEmpty: Boolean = false

  def printElement: String = {
    if(tail.isEmpty) "" + head
    else head + " " + tail.printElement
  }

  override def enqueue[B >: A](element: B): MyQueue[B] = ConsQueue(this.head, this.tail.enqueue(element))

  override def dequeue(): MyQueue[A] = if(tail.isEmpty) EmptyQueue else tail //ConsStack(tail.peek(), this.tail - this.tail.peek())

  override def peek(): A = head
}

object MyQueueTest extends App {
  val queue = ConsQueue("Joy", ConsQueue("Matt", EmptyQueue))
  val anotherQueue = queue.enqueue("Pavel").enqueue("Samir")
  println(anotherQueue.peek())
  println(anotherQueue.dequeue().dequeue().dequeue().dequeue() toString())

}
