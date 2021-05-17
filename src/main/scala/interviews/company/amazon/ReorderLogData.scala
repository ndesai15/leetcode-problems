package interviews.company.amazon

/**
  * @author ndesai on 2021-04-19
  *
 */
 
 
object ReorderLogData {

  /*
     Input: logs = ["dig1 8 1 5 1","let1 art can","dig2 3 6","let2 own kit dig","let3 art zero"]
     Output: ["let1 art can","let3 art zero","let2 own kit dig","dig1 8 1 5 1","dig2 3 6"]

     Problem Statement: https://leetcode.com/problems/reorder-data-in-log-files
   */
  def reOrderLogData(logs: List[String]): List[String] = {
    val(digitLogs: List[String], letterLogs: List[String]) = logs.partition(log => log(log.indexOf(" ")+1).isDigit)

    implicit val newOrdering: Ordering[String] = Ordering.fromLessThan{ (a,b) =>
      val aList = a.split(" ").toList
      val bList = b.split(" ").toList

      if (aList.tail.mkString(" ").compareTo(bList.tail.mkString(" ")) == 0)
        aList.head.compareTo(bList.head) < 0
      else aList.tail.mkString(" ").compareTo(bList.tail.mkString(" ")) < 0
    }

    letterLogs.sorted ++ digitLogs
  }

  def main(args: Array[String]): Unit = {
    println(reOrderLogData(List("dig1 8 1 5 1", "let1 art can", "dig2 3 6", "let2 own kit dig", "let3 art zero")))
  }

}
