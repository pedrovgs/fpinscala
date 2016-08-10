package fpinscala.chapter2

import scala.annotation.tailrec

object Chapter2 {

  // fib(0) = 0, fib(1) = 1, fib(2) = 1, fib(3) = 2, fib(4) = 3, fib(5) = 5
  def fib(n: Int): Int = {

    @tailrec
    def goFib(n: Int, previous: Int = 0, next: Int = 1): Int = {
      n match {
        case 0 => previous
        case 1 => next
        case _ => goFib(n - 1, next, next + previous)
      }
    }

    goFib(n, 0, 1)
  }

  def nonTailFib(n: Int): Int = {
    if (n <= 1) n
    else nonTailFib(n - 1) + nonTailFib(n - 2)
  }

  def main(args: Array[String]): Unit = {
    println(fib(0))
    println(nonTailFib(0))
    println(fib(1))
    println(nonTailFib(1))
    println(fib(2))
    println(nonTailFib(2))
    println(fib(3))
    println(nonTailFib(3))
    println(fib(4))
    println(nonTailFib(4))
    println(fib(5))
    println(nonTailFib(5))
    println(fib(1000000))
    println(nonTailFib(1000000)) //This generates a stack overflow.
  }
}
