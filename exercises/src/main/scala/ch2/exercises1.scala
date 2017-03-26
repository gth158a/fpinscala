
// Code 2.4.1
// factorial definition
def factorial(n: Int): Int = {
  // local definition or inner function
  @annotation.tailrec
  def go(n: Int, acc: Int): Int =
    if (n <= 0) acc
    else go(n-1, n*acc)
  go(n, 1)
}



/** Exercise 2.1
Write a recursive function to get the nth Fibonacci number
(http://mng.bz/C29s).
The first two Fibonacci numbers are 0 and 1.
The nth number is always the sum of the previous two—the sequence
begins 0, 1, 1, 2, 3, 5.
Your definition should use a local tail-recursive function.
*/
def fib(n: Int): Int = {
@annotation.tailrec
  def prev(n:Int, fn:Int, sn: Int): Int =
    if (n <= 3) fn + sn
    else prev(n-1, sn, fn + sn)
  prev(n, 0, 1)
}


// Code 2.4.2

//first higher order function
object MyModule {

// definitions of abs and factorial goes here
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n, 1)
  }

// functions to be refactored with formatResult
  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  // function that takes care of formatting Abs and Factorial
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    // with old function
    println(formatAbs(-42))
    println(formatFactorial(7))
    // with new function
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
  }
}
