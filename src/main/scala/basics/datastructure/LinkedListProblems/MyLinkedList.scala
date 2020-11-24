package basics.datastructure.LinkedListProblems

/**
  * @author ndesai on 2020-11-11
  * Problem Statement: Build own linked list
 */

abstract class MyLinkedList[+A] {

  /*
     head = the first element of the list
     tail = the remainder of the list
     isEmpty = is this list empty
     add(element) => returns a new list with this element added
     toString = String representation of the list
     TODO: Insert an element to a specific index
   */

  def head: A
  def tail: MyLinkedList[A]
  def isEmpty : Boolean
  def prepend[B >: A](element: B) : MyLinkedList[B] // make this list an immutable list
  def append[B >: A](element: B): MyLinkedList[B]
  def ++[B >: A](anotherList: MyLinkedList[B]): MyLinkedList[B]
  def insert[B >:A](index: Int, element:B): MyLinkedList[B]

  // polymorphic call
  def printElements: String
  override def toString: String = s"[$printElements]"
}

case object Empty extends MyLinkedList[Nothing] {

  def head: Nothing = throw new NoSuchElementException("No Such Element")
  def tail: MyLinkedList[Nothing] = throw new NoSuchElementException("No Such Element")
  def isEmpty : Boolean = true
  def prepend[B >: Nothing](element: B): MyLinkedList[B] = Cons(element, this) // make this list an immutable list
  def append[B >: Nothing](element:B): MyLinkedList[B] = Cons(element, this)
  override def printElements: String = ""
  def ++[B >: Nothing](anotherList: MyLinkedList[B]): MyLinkedList[B] = anotherList
  override def insert[B >: Nothing](index: Int, element:B): MyLinkedList[B] = Cons(element,this)

}

case class Cons[+A](h: A, t: MyLinkedList[A]) extends MyLinkedList[A] {

  def head: A = h
  def tail: MyLinkedList[A] = t
  def isEmpty : Boolean = false
  def prepend[B >: A](element: B) : MyLinkedList[B] = Cons(element, this)
  def ++[B >: A](anotherList: MyLinkedList[B]) = Cons(h, t ++ anotherList)

  /*
     [1 2 3] append 4
       => Cons(1, [2 3] ++ Cons(4, Empty))
       => Cons(1, Cons(2, [3] ++ Cons(4, Empty)))
       => Cons(1, Cons(2, Cons(3, Cons(4, Empty))))
  */

  def append[B >: A](element: B): MyLinkedList[B] = Cons(h, t ++ Cons(element, Empty))

  override def printElements: String =
    if (t.isEmpty) "" + h
    else h + " " + t.printElements

  override def insert[B >: A](index: Int, element: B): MyLinkedList[B] = {
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

