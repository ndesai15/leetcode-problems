/**
  * @author ndesai on 2020-10-17
  *
 */
 
 
object Jaybahuchar extends App {
  println("Jay bahuchar mataji")

  /*
  val list = List(2,1,2,65,34,2,16,17)
  val list2 = List('z','a','x','y','m','c','d','b')
  val list3 = List("abv","xcz","bcs","abc")
  println(list)
  println(list.sorted)
  println(list2.sorted)
  println(list3.sorted)


  val current = System.currentTimeMillis()
  for {
    i <- 1 to 100
  } yield {
    if(i % 15 == 0) println("fizzbuzz")
    else if (i%3 ==0) println("fizz")
    else if (i%5 == 0) println("buzz")
    else println(i)
  }
  println(s"Time in module: ${System.currentTimeMillis() - current}") */

  val current2 = System.currentTimeMillis()
  var c3 = 0
  var c5 = 0
  var d = ""
  for{
    i <- 1 to 100
  } yield {
    c3 = c3 + 1
    c5 = c5 + 1
    if(c3 == 3) {
      d = s"${d}fizz"
      c3 = 0
    }
    if(c5 == 5) {
      d = s"${d}buzz"
      c5 = 0
    }
    if(d == "") println(i)
    else println(d)



  }
  println(s"Time Without module: ${System.currentTimeMillis() - current2}")

}
