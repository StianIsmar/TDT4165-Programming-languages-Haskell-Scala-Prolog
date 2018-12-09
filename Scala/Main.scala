object Hello extends App {


  val x: Array[Int] = Array(1, 2, 3)
  println(s"There are ${x.length} elements")
  for (i <- x) println(i)


  // 1b)

  var generated: Array[Int] = Array()
  for (i <- 1 to 50) generated :+= i

  // 1c)
  // Append to the array values 51 to 100 using MAP and anon func
  (51 to 100).map((x: Int) => generated :+= x)
  println("generated" + generated.mkString(" "))


  //d)
  // Create a function that sums the elements in an array of integers
  // using a for-loop
  def sumElements(input: Array[Int]): Int ={
    var counter: Int = 0
    for (i <- input) counter += i
    counter
  }

  //e)
  // Summing elements using recursion!

  def recSum(myArray: Array[Int]): Int = {
    myArray match {
      case myArray.isEmpty => 0
      case _ => x.head + recSum(x.tail)
    }
  }

  def sum_recursive(input: Array[Int]): Int = {
    if (input.isEmpty) {
      0
    } else {
      input {
        0
      } + sum_recursive(input.drop(1))
    }
  }

    var xs: Array[Int] = Array(1, 2, 3, 4, 5)
      println(recSum(xs) + " DETTE ER RECSUM!!")



}
