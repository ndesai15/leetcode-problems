package com.rockthejvm.graphs

/**
  * @author ndesai on 2021-04-15
  *
 */
 
 
object TownJudge {

  /*
     n people, 1 to n
     trust = List[(a, b)]
     (a, b) = a trusts b

     There might be a town judge.
       The town judge trusts nobody. == outdegree(tj) = 0
       Everybody (except for the town judge) trusts the town judge. == indegree(tj) == n-2
       There is exactly one person that satisfies these properties.

     Find the town judge, or return -1
   */

  def findJudge(n: Int, trust: List[(Int, Int)]): Int = {
    val inDegree: Map[Int, Int] = trust.foldLeft(Map[Int, Int]()){
      case (map, (_, b)) => map + (b -> (map.getOrElse(b, 0) + 1))
    }

    val outDegrees: Map[Int, Int] = trust.foldLeft(Map[Int, Int]()) {
      case (map, (a, b)) => map + (a -> (map.getOrElse(a, 0) + 1))
    }

    val townJudgeOption: Option[Int] = (1 to n).find{ person =>
      inDegree.getOrElse(person, 0) == n - 1 && outDegrees.getOrElse(person, 0) == 0
    }

    townJudgeOption.getOrElse(-1)
  }

  def main(args: Array[String]): Unit = {
    println(findJudge(2, List((1,2)))) // 2
    println(findJudge(3, List((1,2), (3,2)))) // 2
    println(findJudge(3, List((1,2), (2,3), (3,1)))) // -1
    println(findJudge(4, List((1,3),(2,3),(1,4),(2,4),(3,4)))) // 4
  }
}
