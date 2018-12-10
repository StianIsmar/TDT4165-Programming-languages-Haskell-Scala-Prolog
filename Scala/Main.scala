import Array._
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
  def sumElements(input: Array[Int]): Int = {
    var counter: Int = 0
    for (i <- input) counter += i
    counter
  }

  //e)
  // Summing elements using recursion!


  def sum_recursive(input: Array[Int]): Int = {
    if (input.isEmpty) {
      0
    } else {
      input {
        0
      } + sum_recursive(input.drop(1))
    }
  }

  var xss: Array[Int] = Array(1, 2, 3, 4, 5)
  println(sum_recursive(xss) + " DETTE ER RECSUM!!")


  /** * f) Fibonacci: **/
  // BigInt vs Int

  def fib(n: BigInt): BigInt = {
    if (n == 0 || n == 1) {
      n
    }
    else {
      fib(n - 1) + fib(n - 2)
    }
  }

  /** With pattern matching **/
  def fib1(x: BigInt): BigInt = x match {
  case x if x==0 => 0
  case x if x==1 => 1
  case _ => fib1 (x - 1) + fib1 (x - 2)
  }


  println(fib(6) + "Dette er fib med if")
  println(fib1(6) + "Dette er fib med matching")

  /** Task 2 **/
  //2.a) What is this:
  def my_func(f: () => BigInt, b: Boolean) = {
    lazy val t = f()
    if (b) println(t)
  }

  // The input for this function is the functino f, which is a function which takes no arguments,
  // but returns a BigInt
  // my_func also takes in a boolean with the variablename b. If the boolean is True,
  // the function will evaluate the line and print t.

  //2b)
  //Same function as above, but with val instead of lazy val:
  // The val is no evaluated straight away
  def myFunc2(f: () => BigInt, b: Boolean) = {
    val t = f()
    if (b) println(t)}
    // The val is now executed straight away, whereas the lazy val is executed only when it is need the first
    // time

    //2c)
    //Why do I think it is helpful to use lazy eval? Same memory for when you actually need it!
    // Infinite lists and streams

    /** Task 3: Concurrency in Scala **/
    //3a)
    // Create a function that takes as argument a function and returns a Thread  initialized
    // with the input function
    //Make sure that the returned thread is not started!

    def createThread(f: () => Unit): Thread = {
      // This creates a thread by overriding a class method with another implementation. The thread is not yet started.
      new Thread {
        override def run {
          f()
        }
      }
    }
  //evt:


}
