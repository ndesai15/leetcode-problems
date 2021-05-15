object Solution {
  import scala.collection.immutable.HashSet
  def orangesRotting(grid: Array[Array[Int]]): Int = {

    def orangesRotting(freshHashSet: HashSet[(Int, Int)], returnValue: Int): Int = {
      if (freshHashSet.isEmpty) returnValue
      else {
        val newRotten: Set[(Int, Int)] = freshHashSet.filter(coordinates => hasRottenNeighbor(coordinates._1, coordinates._2))

        val updatedFresh: HashSet[(Int, Int)] = freshHashSet -- newRotten

        if (newRotten.isEmpty) -1
        else {
          newRotten.foreach(coordinates => grid(coordinates._1)(coordinates._2) = 2)
          orangesRotting(updatedFresh, returnValue + 1)
        }
      }
    }

    def hasRottenNeighbor(row: Int, column: Int): Boolean = {
      Seq(
        (row - 1, column),
        (row + 1, column),
        (row, column - 1),
        (row, column + 1)
      ).exists(coordinates => {
        val (row: Int, column: Int) = coordinates
        row >= 0 && column >= 0 && row < grid.length && column < grid(row).length && grid(row)(column) == 2
      })
    }

    if (grid.isEmpty) -1
    else {
      val temp = for {
        row <- grid.indices
        column <- grid(row).indices
        if grid(row)(column) == 1
      } yield (row, column)

      orangesRotting(
        freshHashSet = HashSet[(Int, Int)](temp: _*),
        returnValue = 0
      )
    }

  }

  def main(args: Array[String]): Unit = {
    println(orangesRotting(Array(Array(2,1,1), Array(1,1,0), Array(0,1,1))))
    //println(orangesRotting(Array(Array(0,2))))
  }




}