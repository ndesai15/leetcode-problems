package interviews.datastructure.StackQueuesProblems

/**
  * @author ndesai on 8/7/21
  *
 */
 
 
sealed trait RStack[+T] {
  def top: T
  def bottom: RStack[T]
  def push[S >: T](element: S): RStack[S] = new ConsStack[S](element, this)
  def pop(): (T, RStack[T])
  def peek(): T
  def isEmpty(): Boolean
}

case object EmptyStack extends RStack[Nothing] {
  override def top: Nothing = throw new NoSuchElementException
  override def bottom: RStack[Nothing] = throw new NoSuchElementException
  override def pop(): (Nothing, RStack[Nothing]) = (throw new NoSuchElementException, this)
  override def peek(): Nothing = throw new NoSuchElementException
  override def isEmpty(): Boolean = true
}

case class ConsStack[T](override val top:T, override val bottom: RStack[T]) extends RStack[T] {
  override def pop(): (T, RStack[T]) = (top, this.bottom)
  override def peek(): T = this.top
  override def isEmpty(): Boolean = false
}

object JaybahucharStackTest extends App {
  val stack = EmptyStack.push("Google")
  println(stack.peek())  // Google
  val discordStack  = stack.push("Discord")
  println(discordStack.peek())  // Discord
  val youtubeStack = discordStack.push("youtube")
  val (element, youtubePoppedStack) = youtubeStack.pop()
  println(element) // youtube
  println(youtubePoppedStack.peek()) // Discord
}

