package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val result = rng.nextInt
    if (result._1 >= 0) result
    else ((result._1 + 1) * -1, result._2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (value, random) = nonNegativeInt(rng)
    if (value == Int.MaxValue) {
      ((value - 1) / Int.MaxValue.toDouble, random)
    } else {
      (value/Int.MaxValue.toDouble, random)
    }
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, nextRng )= rng.nextInt
    val (d, nextDoubleRng) = double(nextRng)
    ((i, d), nextDoubleRng)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (t, r) = intDouble(rng)
    (t.swap, r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, nextDoubleRng1) = double(rng)
    val (d2, nextDoubleRng2) = double(nextDoubleRng1)
    val (d3, nextDoubleRng3) = double(nextDoubleRng2)
    ((d1, d2, d3), nextDoubleRng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(count: Int, rng: RNG, l: List[Int]): (List[Int], RNG) = {
      if (count == 0) (l, rng)
      else {
        val (nextInt, nextRng) = rng.nextInt
        go(count -1, nextRng, nextInt :: l)
      }
    }
    go(count,rng, List[Int]())
  }

  def double2(rng: Rand[Double]): Rand[Double] = map(nonNegativeInt)(_ / Int.MaxValue.toDouble + 1)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
