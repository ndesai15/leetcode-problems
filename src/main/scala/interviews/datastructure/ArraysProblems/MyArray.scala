package interviews.datastructure.ArraysProblems

/**
  * @author ndesai on 2020-10-24
  *
 */
 

abstract class MyArray[+A] {

  /*
      head = first element of the Array
      tail = remainder of the list
      push(element) => add new element to the list (add at the end for now)
      pop() => delete last element
      delete(index) => delete an element at given index

  */

  def head: A
  def tail: MyArray[A]
  def push[B >: A](element: B): MyArray[B]
  def pop(): MyArray[A]
  def delete(index: Int): MyArray[A]

}

case object Empty extends MyArray[Nothing]{

  def head: Nothing = throw new NoSuchElementException("No head for empty array")
  def tail: MyArray[Nothing] = throw new NoSuchElementException("No tail for empty array")
  def push[B >: Nothing](element: B): MyArray[B] = new Cons(element, Empty)
  def pop(): MyArray[Nothing] = Empty
  def delete(index: Int): MyArray[Nothing] = Empty

}

case class Cons[+A](h: A, t: MyArray[A]) extends MyArray[A]{
  def head: A = h
  def tail: MyArray[A] = t
  def push[B >: A](element: B): MyArray[B] = ???

  def pop(): MyArray[A] = ???
  def delete(index: Int): MyArray[A] = ???
}

