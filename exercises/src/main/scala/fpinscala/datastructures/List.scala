package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil //We could also use "case Nil => sys.error("tail of empty list")" here.
    case Cons(_, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("There is no head to replace")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (_, _) if n <= 0 => l
    case (Nil, _) => Nil
    case (_, _) => drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h,t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, y) => y + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def product3(l: List[Int]) =
    foldLeft(l, 1)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a, g) => b => g(f(b, a)))(z)

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_,_))

  def append3[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(a1, a2)((acc, x) => Cons(x, acc))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(append)

  def appendOne(l: List[Int]): List[Int] = foldRight(l, List[Int]())((x, acc) => Cons(x + 1, acc))

  def toListString(l: List[Double]): List[String] = foldRight(l, List[String]())((x, acc) => Cons(x.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]())((x, acc) => Cons(f(x), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
     foldRight(l, List[A]()) { (x, acc) => {
        if (f(x)) Cons(x, acc)
        else acc
       }
     }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)((a: A) =>
    if (f(a)) List(a)
    else Nil
  )

  def sumPairs(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Nil, Nil) => Nil
    case (xs, Nil) => xs
    case (Nil, xs) => xs
    case (Cons(y, ys), Cons(x, xs)) => Cons(y + x, sumPairs(ys, xs))
  }

  def zipWith[A](a: List[A], b: List[A])(f: (A,A) => A): List[A] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  @tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_,Nil) => true
    case (Cons(h, t),Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_,t) => hasSubsequence(t, sub)
  }


}
