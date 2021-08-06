package interviews.datastructure.StackQueuesProblems

/**
  * @author ndesai on 8/5/21
  *
 */
 
 
sealed abstract class RStack[+T] {

  // Build Stack Data Structure

  def top: T
  def bottom: RStack[T]

  def peek(): T
  def pop(): RStack[T]
  def push[S >: T](element: S): RStack[S]
  def isEmpty: Boolean
  def ::[S >: T](element: S): RStack[S] = new SCons[S](element, this)
  def ++[S >: T](element: S): RStack[S]
  def removeLastElement(): RStack[T]
  def reverse(): RStack[T]

  def printElement: String

  // polymorphic call
  override def toString(): String = "[" + printElement + "]"
}

case object SEmpty extends RStack[Nothing] {
  override def top: Nothing = throw new NoSuchElementException
  override def bottom: RStack[Nothing] = throw new NoSuchElementException
  override def pop(): RStack[Nothing] = throw new NoSuchElementException
  override def push[S >: Nothing](element: S): RStack[S] = new SCons[S](element, this)
  override def peek(): Nothing = throw new NoSuchElementException
  override def isEmpty: Boolean = true
  override def reverse(): RStack[Nothing] = this
  override def ++[S >: Nothing](element: S): RStack[S] = new SCons[S](element, this)
  override def removeLastElement(): RStack[Nothing] = this
  override def printElement: String = ""
}

case class SCons[+T](override val top:T, override val bottom: RStack[T]) extends RStack[T] {
  def printElement: String = {
    if(bottom.isEmpty) "" + top
    else top + " " + bottom.printElement
  }
  override def isEmpty: Boolean = false
  override def reverse(): RStack[T] = {
    def reverseTailRec(remaining: RStack[T], result: RStack[T]): RStack[T] = {
      if (remaining.isEmpty) result
      else reverseTailRec(remaining.bottom, remaining.top :: result)
    }
    reverseTailRec(this, SEmpty)
  }

  override def ++[S >: T](element: S): RStack[S] = {
    def addTailRec(remaining: RStack[S], result: RStack[S]): RStack[S] = {
      if (remaining.isEmpty) (element :: result).reverse()
      else addTailRec(remaining.bottom, remaining.top :: result)
    }
    addTailRec(this, SEmpty)
  }

  override def removeLastElement(): RStack[T] = {

    def removeTailRec(remaining: RStack[T], result: RStack[T]): RStack[T] = {
      if (remaining.isEmpty) result
      if (remaining.bottom.isEmpty) result
      else removeTailRec(remaining.bottom, remaining.top :: result)
    }
    removeTailRec(this, SEmpty)
  }

  override def push[S >: T](element: S): RStack[S] = {
    new SCons[S](top, this.bottom ++ element)
  }

  override def pop(): RStack[T] =
    if (this.bottom.isEmpty) SEmpty
    else new SCons[T](this.top,this.bottom.removeLastElement())

  override def peek(): T = {
    def peekTailRec(remaining: RStack[T]): T = {
      if (remaining.bottom.isEmpty) remaining.top
      else peekTailRec(remaining.bottom)
    }
    if(this.bottom.isEmpty) top
    else peekTailRec(this.bottom)
  }
}

object JayBahucharStack extends App {

  val stack = SCons(1, SEmpty)

  val emptyStack = stack.pop()
  println("Empty Stack is " + emptyStack toString())


  println("Initial Stack: " + stack toString)
  println("Head should of the stack is " + stack.peek()) // Should return 1

  val pushStack2 = stack.push(2) // 1 -> 2
  println(pushStack2.toString())
  println("After pushing 2 to Stack current head is " + pushStack2.peek())  // 2

  println("Push 3 to Stack")
  val againPush = pushStack2.push(3)  // 1 -> 2 -> 3
  println("Current Stack is " + againPush toString)
  println("After pushing 3 to Stack current head is " + againPush.peek()) // 3

  println("Push 4 to Stack")
  val againPush2 = againPush.push(4) // 1 -> 2 -> 3 -> 4
  println("Current Stack is " + againPush2 toString)

  println("Push 5 to Stack")
  val againPush3 = againPush2.push(5) // 1 -> 2 -> 3 -> 4 -> 5
  println("Current Stack is " + againPush3 toString)
  println("Now head is " + againPush3.peek()) //5

  val popTwice = againPush3.pop().pop() // should POP out 5 & 4
  println("After popping out element twice stack is " + popTwice toString)

  val push100 = popTwice.push(100) // 1 -> 2 -> 3 -> 100
  val pop100 = push100.pop() // 1 -> 2 -> 3

  println(pop100 toString()) // 1 -> 2 -> 3
  println(pop100.peek()) // return 3 */
}
