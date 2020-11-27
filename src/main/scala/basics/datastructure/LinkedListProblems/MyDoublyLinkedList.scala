package basics.datastructure.LinkedListProblems

/**
  * @author ndesai on 2020-11-24
  *
 */
 
/*
abstract class MyDoublyLinkedList[+A] {

  /*
     head = the first element of the list
     previous = the previous node of the list
     tail = the remainder of the list
     isEmpty = is this list empty
     add(element) => returns a new list with this element added
     toString = String representation of the list
   */

  def head: A
  def prev: MyDoublyLinkedList[A]
  def tail: MyDoublyLinkedList[A]
  def ++[B >: A](anotherList: MyDoublyLinkedList[B]): MyDoublyLinkedList[B]
  def prepend[B >:A](element:B): MyDoublyLinkedList[B]
  def isEmpty : Boolean
}

case object EmptyDoublyLinkedList extends MyDoublyLinkedList[Nothing] {

  override def head: Nothing = throw new RuntimeException("No such element")
  override def prev: MyDoublyLinkedList[Nothing] = throw new RuntimeException("No such element")
  override def tail: MyDoublyLinkedList[Nothing] = throw new RuntimeException("No such element")
  override def isEmpty : Boolean = true
  override def prepend[B >: Nothing](element: B): MyDoublyLinkedList[B] = ConsDoublyLinkedList(element,this, this)
  override def ++[B >: Nothing](anotherList: MyDoublyLinkedList[B]): MyDoublyLinkedList[B] = anotherList
}


case class ConsDoublyLinkedList[+A](h: A, p: MyDoublyLinkedList[A], t: MyDoublyLinkedList[A]) extends MyDoublyLinkedList[A] {

  override def head: A = h

  override def prev: MyDoublyLinkedList[A] = p

  override def tail: MyDoublyLinkedList[A] = t

  override def isEmpty: Boolean = false


  def ++[B >: A](anotherList: MyDoublyLinkedList[B]) = ConsDoublyLinkedList(h, t, t ++ anotherList)

  override def prepend[B >: A](element: B): MyDoublyLinkedList[B] =
    ConsDoublyLinkedList(element, EmptyDoublyLinkedList ,this(this.h, element,this.tail))
}

object MyDoublyLinkedListTest extends App {

  val list = ConsDoublyLinkedList(1, EmptyDoublyLinkedList,EmptyDoublyLinkedList)
  println(list.prepend(2).tail)



}
*/
