package com.rockthejvm.graphs

/**
  * @author ndesai on 2021-04-23
  *         Problem Statement: https://leetcode.com/problems/number-of-islands/
  *
 */
 
 
object GridIslandsProblem {

  // Using BFS
  def numOfIslands(grid: Array[Array[Char]]): Int = {
    if(grid.isEmpty || grid.length ==0) 0
    else {
      var counter = 0
      var r = 0
      while(r < grid.length) {
        var c = 0
        while (c < grid(r).length){
          if(grid(r)(c) == '1'){
            counter +=1
            bfs(grid, r, c)
          }
          c +=1
        }
        r +=1
      }
      counter
    }
  }

  def bfs(grid: Array[Array[Char]], r: Int, c: Int): Unit = {
    if(r >=0 && c >=0 && r < grid.length && c < grid(r).length && grid(r)(c) == '1') {
      grid(r)(c) = '0'
      bfs(grid, r - 1, c)
      bfs(grid, r + 1, c)
      bfs(grid, r, c - 1)
      bfs(grid, r, c + 1)
    }
    else ()
  }

  def main(args: Array[String]): Unit = {
    println(numOfIslands(Array(Array('1', '1', '1', '1', '0'),
      Array('1','1','0', '1', '0'),
      Array('0', '0','1','0','0'),
      Array('0', '0', '0', '1', '1'))))

  }
}
