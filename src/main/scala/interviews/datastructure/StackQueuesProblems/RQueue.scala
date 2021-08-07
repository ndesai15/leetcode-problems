package interviews.datastructure.StackQueuesProblems

/**
  * @author ndesai on 8/7/21
  *
 */
 
 
abstract class RQueue[+T] {
  def first: T
  def last: RQueue[T]
  def enqueue[S >: T](element: S): RQueue[S]
  def dequeue(): (T, RQueue[T])
  def peek(): T
  def ++[S >: T](element: S): RQueue[S]
  def isEmpty(): Boolean
}

case object EmptyQueue2 extends RQueue[Nothing] {
  override def first: Nothing = throw new NoSuchElementException
  override def last: RQueue[Nothing] = throw new NoSuchElementException
  override def dequeue(): (Nothing, RQueue[Nothing]) = (throw new NoSuchElementException, this)
  override def isEmpty(): Boolean = true
  override def enqueue[S >: Nothing](element: S): RQueue[S] = new QueueCons[S](element, this)
  override def peek(): Nothing = throw new NoSuchElementException
  override def ++[S >: Nothing](element: S): RQueue[S] = new QueueCons[S](element, this)
}

case class QueueCons[+T](override val first: T, override val last: RQueue[T]) extends RQueue[T] {
  override def dequeue(): (T, RQueue[T]) = (this.first, new QueueCons[T](this.last.first, this.last.last))
  override def peek(): T = this.first
  def ++[S >: T](element: S): RQueue[S] = {
    if (this.isEmpty()) new QueueCons[S](element,this)
    else new QueueCons[S](this.first, this.last ++ element)
  }
  override def isEmpty(): Boolean = false
  override def enqueue[S >: T](element: S): RQueue[S] = new QueueCons[S](first, this.last ++ element)
}

object RQueue extends App {
  val queue = new QueueCons("Matt", new QueueCons("Samir", EmptyQueue2))
  val newQueue = queue.enqueue("Pavel").enqueue("Joy")
  println(queue.peek()) // Matt
  println(queue.isEmpty()) // false
  println(queue.dequeue()._2.peek()) // Samir -> Pavel -> Joy
  println(newQueue.dequeue()._2.dequeue()._2.peek()) // true
}
