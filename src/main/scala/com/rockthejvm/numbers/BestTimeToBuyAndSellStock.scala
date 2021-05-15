package com.rockthejvm.numbers

/**
  * @author ndesai on 2021-04-26
  *         Problem Statement: https://leetcode.com/problems/best-time-to-buy-and-sell-stock/
  *
 */
 
 
object BestTimeToBuyAndSellStock {

  def maxProfit(prices: Array[Int]): Int = {
    prices.foldLeft(prices.head, 0) {
      (pair, price) => (Math.min(pair._1, price), Math.max(pair._2, price - pair._1))
    }._2
  }

  def main(args: Array[String]): Unit = {
    println(maxProfit(Array(7,1,5,3,6,4)))
    println(maxProfit(Array(7,6,4,3,1)))
  }

}
