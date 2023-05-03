package chapter2

@main def main() : Unit = {
  // exercise 2.1
  val n = 6
  println("fib of " + n + " = " + fib(n))

  // exercise 2.2
  val int_arr_unsorted : Array[Int] = Array(2, 5, 0, -3, 9)
  println("is the array sorted: " + isSorted(int_arr_unsorted, (a, b) => a < b))
  val int_arr_sorted : Array[Int] = Array(1, 4, 7, 8)
  println("is the array sorted: " + isSorted(int_arr_sorted, (a, b) => a < b))
  val string_arr_unsorted : Array[String] = Array("hello", "world", "of", "cats")
  println("is the array sorted: " + isSorted(string_arr_unsorted, (a, b) => a < b))
  val string_arr_sorted: Array[String] = Array("a", "b", "c", "d")
  println("is the array sorted: " + isSorted(string_arr_sorted, (a, b) => a < b))



}

  // exercise 2.3
def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
  (a: A) => (b: B) => f(a, b)
}

def plus(x: Int, y : Int) = {
  x + y
}

val plus3 = curry(plus)(3)

def concat(a: String, b:String) : String = {
  a + b
}

val pad3 = curry(concat)("   ")

// exercise 2.4
def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
  (a: A, b: B) => f(a)(b)
}

// exercise 2.5
def compose[A,B,C](f: B => C, g: A => B): A => C = {
  (a: A) => f(g(a))
}

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  def loop(n : Int) : Boolean = {
    if(n >= as.length - 1) true
    else if(!ordered(as(n), as(n + 1))) false
    else loop(n + 1)
  }

  loop(0)
}

def fib(n: Int): Int = {
  @annotation.tailrec
  def go(prev: Int, next: Int, current:Int): Int = {
    if (current <= 0) prev
    else go(next, prev + next, current - 1)
  }

  go(0, 1, n)
}
