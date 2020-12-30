package interviews.datastructure.LinkedListProblems

/**
  * @author ndesai on 2020-11-11
  * Problem Statement: Build own linked list
 */

abstract class MySinglyLinkedList[+A] {

  /*
     head = the first element of the list
     tail = the remainder of the list
     isEmpty = is this list empty
     add(element) => returns a new list with this element added
     toString = String representation of the list
   */

  /*

    1. Accessing head & tail : O(1)
    2. Appending an element, inserting element = O(n)

  */
  def head: A
  def tail: MySinglyLinkedList[A]
  def isEmpty : Boolean
  def prepend[B >: A](element: B) : MySinglyLinkedList[B] = Cons(element, this)
  def append[B >: A](element: B): MySinglyLinkedList[B]
  def ++[B >: A](anotherList: MySinglyLinkedList[B]): MySinglyLinkedList[B]
  def insert[B >:A](index: Int, element:B): MySinglyLinkedList[B]

  // polymorphic call
  def printElements: String
  override def toString: String = s"[$printElements]"
}

case object Empty extends MySinglyLinkedList[Nothing] {

  def head: Nothing = throw new NoSuchElementException("No Such Element")
  def tail: MySinglyLinkedList[Nothing] = throw new NoSuchElementException("No Such Element")
  def isEmpty : Boolean = true
  def append[B >: Nothing](element:B): MySinglyLinkedList[B] = Cons(element, this)
  override def printElements: String = ""
  def ++[B >: Nothing](anotherList: MySinglyLinkedList[B]): MySinglyLinkedList[B] = anotherList
  override def insert[B >: Nothing](index: Int, element:B): MySinglyLinkedList[B] = Cons(element,this)

}

case class Cons[+A](h: A, t: MySinglyLinkedList[A]) extends MySinglyLinkedList[A] {

  def head: A = h
  def tail: MySinglyLinkedList[A] = t
  def isEmpty : Boolean = false

  def ++[B >: A](anotherList: MySinglyLinkedList[B]) = Cons(h, t ++ anotherList)

  /*
     [1 2 3] append 4
       => Cons(1, [2 3] ++ Cons(4, Empty))
       => Cons(1, Cons(2, [3] ++ Cons(4, Empty)))
       => Cons(1, Cons(2, Cons(3, Cons(4, Empty))))
  */

  def append[B >: A](element: B): MySinglyLinkedList[B] = Cons(h, t ++ Cons(element, Empty))

  override def printElements: String =
    if (t.isEmpty) "" + h
    else h + " " + t.printElements

  override def insert[B >: A](index: Int, element: B): MySinglyLinkedList[B] = {
    if (index < 0) {
      Empty
    }
    else {
      if(index == 0) Cons(element, this)
      else {
         Cons(head, t.insert(index-1,element))
      }
    }
  }
}

object LinkedListTest extends App {
  val list = Cons(11, Cons(22, Cons(33, Cons(44, Empty))))
  println(list toString)   // 11 -> 22 -> 33 -> 44
  println(s"After insert ${list.insert(3,1000).toString}") // 11 -> 22 -> 33 -> 1000 -> 44
}

