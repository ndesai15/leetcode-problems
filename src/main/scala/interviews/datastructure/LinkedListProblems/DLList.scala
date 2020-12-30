package interviews.datastructure.LinkedListProblems

/**
  * @author ndesai on 2020-12-06
  *
 */

// Forward reference technique
trait DLList[+T]{
  def value: T
  def prev: DLList[T]
  def next: DLList[T]
  def prepend[S >: T](element: S): DLList[S]
  def append[S >: T](element: S): DLList[S]

  def updatePrev[S >: T](newPrev: => DLList[S]): DLList[S]
  def updateNext[S >: T](newNext: => DLList[S]): DLList[S]
}

object DLEmpty extends DLList[Nothing] {
  override def value: Nothing = throw new NoSuchElementException("value of DLList empty")
  override def prev: DLList[Nothing] = throw new NoSuchElementException("head of DLList empty")
  override def next: DLList[Nothing] = throw new NoSuchElementException("next of DLList empty")
  override def append[S >: Nothing](element: S): DLList[S] = new DLCons(element, DLEmpty, DLEmpty)
  override def prepend[S >: Nothing](element: S): DLList[S] = new DLCons(element, DLEmpty, DLEmpty)

  def updatePrev[S >: Nothing](newPrev: => DLList[S]): DLList[S] = this
  def updateNext[S >: Nothing](newNext: => DLList[S]): DLList[S] = this
}

class DLCons[+T](override val value: T, p: => DLList[T], n: => DLList[T]) extends DLList[T] {
  // implement methods
  // new Node <-> 1 - 2 - 3 -

  override lazy val prev: DLList[T] = p
  override lazy val next: DLList[T] = n

  // 3 - 4 - 5
  // updatePrev with 1
  // 1 <---- 3 - 4 - 5
  override def updatePrev[S >: T](newPrev: => DLList[S]): DLList[S] = {
    lazy val result: DLList[S] = new DLCons[S](value, newPrev, n.updatePrev(result)) // Forward propagation
    result
  }


  // 3 - 4 - 5
  override def updateNext[S >: T](newNext: => DLList[S]): DLList[S] = {
    lazy val result:DLList[S] = new DLCons[S](value, p.updatePrev(result), newNext)
    result
  }

  // 1 - 2 - 3 - 4
  //               - 5
  override def append[S >: T](element: S): DLList[S] = {
    lazy val result: DLList[S] = new DLCons[S](value, p.updateNext(result), n.append(element).updatePrev(result))
    result
  }

  override def prepend[S >: T](element: S): DLList[S] = {
    lazy val result: DLList[S] = new DLCons[S](value, p.prepend(element).updateNext(result), n.updatePrev(result))
    result
  }


}

// CBN and lazy vals

object DLListTest extends App {

  // - 3 - 1 - 2 - 4 -
  val list = DLEmpty.prepend(1).append(2).prepend(3).append(4)
  println(list.value) // 1
  println(list.next.value) // 2
  println(list.prev.value) // 3
  println(list.next.prev == list) // true
  println(list.next.next.value) // 4
  println(list.prev.next == list) // true
  println(list.next.next.prev.prev == list) // true
}

