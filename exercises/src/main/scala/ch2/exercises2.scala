
// Code 2.5.1
// Listing 2.3
def findFirst(ss: Array[String], key: String): Int = {
  // If n is past the end of the @annotation.tailrec array, return -1,
  // indicating the key doesn’t exist in the array.
  @annotation.tailrec
  def loop(n: Int): Int =
    if (n >= ss.length) -1
    else if (ss(n) == key) n
    else loop(n + 1)
  loop(0)
}


// polymorphic functions work with multiple data types
// Listing 2.4
def findFirst[A](as: Array[A], p:A => Boolean): Int = {
  // If n is past the end of the @annotation.tailrec array, return -1,
  // indicating the key doesn’t exist in the array.
  @annotation.tailrec
  def loop(n: Int): Int =
    if (n >= as.length) -1
    else if  (p(as(n))) n
    else loop(n + 1)
  loop(0)
}

// Exercise 2.2 
// Implement isSorted, which checks whether an Array[A] is sorted according to
// a given comparison function

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  def loop(n: Int, first: A, second: B): Int =
    if (n >= as.length) true
    else if (gt(first, second)) false
    else loop(n + 1, as(n), as(n+1) )
  loop(0, as(0), as(1))
}
