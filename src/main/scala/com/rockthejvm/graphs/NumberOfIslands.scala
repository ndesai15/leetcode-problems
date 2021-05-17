package com.rockthejvm.graphs

/**
  * @author ndesai on 2021-04-21
  *
 */
 
 
object NumberOfIslands {

  def numIslands(grid: Array[Array[Char]]): Int = {

    def dfs(grid: Array[Array[Char]], r: Int, c: Int): Unit = {
      // count number of islands
      val numberOfRows = grid.length
      val numnberOfColumns = grid(0).length

      if(r < 0 || c < 0 || r >= numberOfRows || c >= numnberOfColumns) ()
      else {
        grid(r)(c) = '0'
        dfs(grid, r - 1, c)
        dfs(grid, r + 1, c)
        dfs(grid, r, c - 1)
        dfs(grid, r, c + 1)
      }
    }

    if (grid.length == 0 || grid == null) 0
    else {
      // count number of islands
      val numberOfRows = grid.length
      val numnberOfColumns = grid(0).length
      var countIslands = 0
      var r = 0

      while (r < numberOfRows) {
        var c =0
        while (c < numnberOfColumns) {
          if (grid(r)(c) == '1') {
            // do dfs & increase counter
            countIslands += 1
            dfs(grid, r, c)
          }
          c +=1
        }
        r +=1
      }
      countIslands
    }
  }
}
