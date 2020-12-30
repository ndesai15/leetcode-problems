package interviews.datastructure.ArraysProblems

/**
  * @author ndesai on 2020-10-20
  *
 */


object ArrayBasics extends App {

  val array:Array[Int] = Array(1,2,3,4,5) // 5 * 4 = 20 bytes of data
                            // 0 1 2 3 4

  // 1. Access
  val second = array(2)  // O(1)

  // 2. push
  val array2 = array :+ 5 // O(n)

  val array3: collection.Seq[Int] = 0 +: array // O(n)

  // 3. pop
  array /** There is no such method supported for deleting element from array.
            First convert array to arraybuffer & then remove element , convert it back to array
        */

  // 4. Replacement
  array.update(2, 200) // O(1)
  array(3) = 100 // Translated to array.update(3, 100)

  array.foreach(println)

  // 5. map
  array.map(_ * 2).foreach(println)

  val seqReversed = array.reverse



}
