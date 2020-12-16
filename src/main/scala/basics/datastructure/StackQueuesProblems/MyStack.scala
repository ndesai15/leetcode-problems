package basics.datastructure.StackQueuesProblems

/**
  * @author ndesai on 2020-11-27
  *
 */
 
 
abstract class MyStack[+A] {

  /*
     Implement a Stack as a linked list

     LIFO = LAST IN FIRST OUT

     isEmpty = Stack is Empty or not
     push = push an element to stack
     pop = pop last element out of stack
     peek = return first element of stack

   */

  def isEmpty: Boolean
  def push[B >:A](element: B): MyStack[B]
  def pop():MyStack[A]
  def peek():A

  def printElement: String
  // polymorphic call
  override def toString(): String = "[" + printElement + "]"

}

case object EmptyStack extends MyStack[Nothing]{

  override def isEmpty: Boolean = true

  override def push[B >: Nothing](element: B): MyStack[B] = ConsStack(element,this)

  override def pop(): MyStack[Nothing] = throw new NoSuchMethodException("You can't pop from Empty Stack")

  override def peek(): Nothing = throw new NoSuchElementException("No element in Empty Stack")

  override def printElement: String = ""
}

case class ConsStack[+A](head: A, tail: MyStack[A]) extends MyStack[A] {

  override def isEmpty: Boolean = false

  def printElement: String = {
    if(tail.isEmpty) "" + head
    else head + " " + tail.printElement
  }

  // TODO Push to the last of the linked list
  override def push[B >: A](element: B): MyStack[B] = {
    ConsStack(element, this)
  }

  override def pop(): MyStack[A] = if(!tail.isEmpty) tail else EmptyStack//ConsStack(tail.peek(), this.tail - this.tail.peek())

  override def peek(): A = head
}

object MyStackTest extends App {

  val stack = ConsStack(1, EmptyStack)

  val emptyStack = stack.pop()
  println("Empty Stack is " + emptyStack toString())

  println("Initial Stack: " + stack toString)
  println("Head should of the stack is " + stack.peek()) // Should return 1

  val pushStack2 = stack.push(2) // 2 -> 1
  println(pushStack2.toString())
  println("Now head should be " + pushStack2.peek())  // 2

  println("Push 3 to Stack")
  val againPush = pushStack2.push(3)  // 3 -> 2 -> 1
  println("Current Stack is " + againPush toString)
  println("Now head is " + againPush.peek()) // 3

  println("Push 4 to Stack")
  val againPush2 = againPush.push(4) // 4 -> 3 -> 2 -> 1
  println("Current Stack is " + againPush2 toString)

  println("Push 5 to Stack")
  val againPush3 = againPush2.push(5) // 5 -> 4 -> 3 -> 2 -> 1
  println("Current Stack is " + againPush2 toString)
  println("Now head is " + againPush3.peek()) //5

  val popTwice = againPush3.pop().pop() // should POP out 5 & 4
  println("Current Stack is " + popTwice toString)

  val push100 = popTwice.push(100) // 100 -> 3 -> 2 -> 1
  val pop100 = push100.pop() // 3 -> 2 -> 1

  println(pop100 toString()) // 3 -> 2 -> 1
  println(pop100.peek()) // return 3

}
