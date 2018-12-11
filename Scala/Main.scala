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

    //3b)
  // Rec func that creates n lambdas
  def createFibLambda(n: BigInt): Array[() => Unit] = n match{
    case n if n <= 0 => Array(() => println(fib1(0)))
    case _ => createFibLambda(n-1) ++ Array(() => println(fib1(n)))
  }
  val threads = createFibLambda(34).map(x => createThread(x))
  println(threads + " Dette er threads ")
   //3c)
  // Map the function in 3a to each lambda in 3c for a value.

  //3d)
  // Map each thread from c) to start:
  threads.map(x => x.start
  threads.map(x => x.join // Makes sure that each thread is executed!
  // The main thread is set on wait.

  //3e)
  // Make code snippet thread safe:
  private var counter: Int = 0

  def increaseCounter(): Int = this.synchronized{ // this.synchornized blocks
    // the other ones
    counter += 1
    counter
  }


  //Med atomic integer:
  private val counter: AtomicInteger = new AtomicInteger
  def increaseCounter: Integer = {
    counter.incrementAndGet
  }



// 3f) DEADLOCK, EXPLAIN AND EXAMPLE
  // A deadlock is a situation we get if two OR MORE threads are waiting for each other to complete an
  // action before proceeding with their own action.
  //They are waiting because each thread has reveived an exclusive access to a resource
  // which the other one needs.

  // In concurrent programming, when two threads obtain two separate monitors
  // at the same time and then attempt to get the others monitor, we get a deadlock.

  // Conditions to get a deadlock: Mutual exclusion, Hold and wait, No preemption, Circular wait
  // Example with lazy val:

  object A {
    lazy val base = 42
    lazy val start = B.step   // (1) A tries to access B
  }

  object B {
    lazy val step = A.base    // (2) B tries to access a lazy val in A
  }

  object DeadlockScenario {
    def run = {
      val result = Future.sequence(Seq(
        Future { A.start },                        // (1)
        Future { B.step }                          // (2)
      ))
      Await.result(result, 1.minute)
    }
  }


  // Example from book:
  /*
  val a = new Account("Stian", 1000)
  val b = new Account("Sara", 2000)
  val t1 = thread { for (i<- 0 until 100) send(a, b, 1) }
  val t2 = thread { for (i<- 0 until 100) send(b, a, 1) }
  t1.join(); t2.join()
  log(s"a = ${a.money}, b = ${b.money}")
}
*/
}
