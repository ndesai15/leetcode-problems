package com.rockthejvm.graphs

import com.rockthejvm.graphs.GraphProblems.{ Graph, findCycle}

/**
  * @author ndesai on 2021-04-15
  *
 */
 
 
object UniCourses {

  /*
     nCourses courses at uni, labeled 0 -> n-1
     prerequisites = List[(a,b)]
     (a,b) = b is required in order to take a

     Can you take all courses 0 .. n-1 without breaking any prereq?
   */

  def canTakeAllCourses(nCourses: Int, prerequisites: List[(Int, Int)]): Boolean = {
    // 0,1,2 ...List[(0,1),(2,0)] --> Map(0 -> Set(1), 2 -> Set(0), 1 -> Set())
    val dependencies: Graph[Int] =
      (0 until nCourses).map(course => (course, Set[Int]())).toMap ++
        prerequisites.foldLeft(Map[Int, Set[Int]]()) {
          case (map, (a,b)) => map + (a -> (map.getOrElse(a,Set()) + b))
        }

    (0 until nCourses).forall(course => findCycle(dependencies, course).isEmpty)
  }

  def findOrder(n: Int, prerequisites: List[(Int, Int)]): List[Int] = {
    // {0 -> Set(2, 3), 1 -> Set(0, 4), 2 -> Set(), 3 -> Set(), 4 -> Set(5), 5 -> Set()}
    val dependencies: Graph[Int] =
      (0 until n).map(course => (course, Set[Int]())).toMap ++
        prerequisites.foldLeft(Map[Int, Set[Int]]()) {
          case (map, (a,b)) => map + (b -> (map.getOrElse(b,Set()) + a))
        }
    /*
        {0 -> Set(2, 3), 1 -> Set(0, 4), 2 -> Set(), 3 -> Set(), 4 -> Set(5), 5 -> Set()}

        ot([0 1 2 3 4 5], [], [], [], []) =
        ot([1 2 3 4 5], [0], [], [], []) =
        ot([1 2 3 4 5], [2 3 0], [0], [], []) =
        ot([1 2 3 4 5], [3 0], [0], [2], [2]) =
        ot([1 2 3 4 5], [0], [0], [3 2], [3 2]) =
        ot([1 2 3 4 5], [], [0], [0 2 3], [0 3 2]) =
        ot([2 3 4 5], [1], [], [0 2 3], [0 3 2]) =
        ot([2 3 4 5], [0 4 1], [1], [0 2 3], [0 3 2]) =
        ot([2 3 4 5], [4 1], [1], [0 2 3], [0 3 2]) =
        ot([2 3 4 5], [5 4 1], [4 1], [0 2 3], [0 3 2]) =
        ot([2 3 4 5], [4 1], [4 1], [0 2 3 5], [5 0 3 2]) =
        ot([2 3 4 5], [1], [1], [0 2 3 4 5], [4 5 0 3 2]) =
        ot([2 3 4 5], [], [], [0 1 2 3 4 5], [1 4 5 0 3 2]) =
        ot([3 4 5], [2], [], [0 1 2 3 4 5], [1 4 5 0 3 2]) =
        ot([3 4 5], [], [], [0 1 2 3 4 5], [1 4 5 0 3 2]) =
          ....

        [1 4 5 0 3 2]
     */
    def orderTailRec(remainingCourses: Set[Int],
                     stack: List[Int],
                     expanding: Set[Int],
                     expanded: Set[Int],
                     order: List[Int]): List[Int] = {
      if(stack.isEmpty)
        if(remainingCourses.isEmpty) order
        else orderTailRec(remainingCourses.tail, List(remainingCourses.head), Set(), expanded, order)
      else {
        val course = stack.head

        if(expanded.contains(course))
          // course node has already been expanded & present in order
          orderTailRec(remainingCourses, stack.tail, expanding, expanded, order)
        else if (expanding.contains(course))
          // course node has been fully expanded (DFS) & ready to move to order
          orderTailRec(remainingCourses, stack.tail, expanding - course, expanded + course, course :: order)
        else{
          val coursesAfter = dependencies(course) // at least empty Set
          if(coursesAfter.exists(neighborCourse => expanding.contains(neighborCourse))) List () // there is cycle in my graph
          else orderTailRec(remainingCourses, coursesAfter.toList ++ stack, expanding + course, expanded, order)
        }
      }
    }
    orderTailRec(dependencies.keySet, List(), Set(), Set(), List())
  }

  def main(args: Array[String]): Unit = {
    println(canTakeAllCourses(2, List((0,1)))) // true
    println(canTakeAllCourses(3, List((0,1),(2,0)))) // true
    println(canTakeAllCourses(2, List((0,1), (1,0)))) // false
    println(findOrder(6, List((0, 1), (2, 0), (3, 0), (4, 1), (5, 4)))) //[1 4 5 0 3 2]
    println(findOrder(3, List((0,1), (1,2), (2,0)))) // List()
    println(findOrder(2, List((0,1)))) // List(1,0)
  }


}
