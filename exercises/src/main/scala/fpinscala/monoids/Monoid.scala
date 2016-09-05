package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = a1 compose a2

    override def zero: (A) => A = a => a
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  import fpinscala.testing._
  import Prop._
  def monoidLaw1[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(gen)((a: A) =>
      m.op(a, m.zero) == a && m.op(m.zero, a) == a)

  def monoidLaw2[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p =>
      m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3))

  def trimMonoid(s: String): Monoid[String] = sys.error("todo")

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldRight(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as match {
    case Nil => m.zero
    case x :: xs => m.op(f(x), foldMap(xs, m)(f))
  }

  def foldMapViaFoldLeft[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((acc, x) => m.op(acc, f(x)))

  def foldMapViaFoldRight[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldRight(m.zero)((x, acc) => m.op(acc, f(x)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.isEmpty) {
      m.zero
    } else if (as.size == 1) {
      f(as(0))
    } else {
      val (first, second) = as.splitAt(as.length / 2)
      m.op(foldMapV(first, m)(f), foldMapV(second, m)(f))
    }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean =
    sys.error("todo")

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = a1.map2(a2)(m.op)

    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap { bs =>
      foldMapV(bs, par(m))(b => Par.lazyUnit(b))
    }

  val wcMonoid: Monoid[WC] = sys.error("todo")

  def count(s: String): Int = sys.error("todo")

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    sys.error("todo")

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    sys.error("todo")

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    sys.error("todo")

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    sys.error("todo")
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    sys.error("todo")

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    sys.error("todo")

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    sys.error("todo")

  def toList[A](as: F[A]): List[A] =
    sys.error("todo")
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    sys.error("todo")
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    sys.error("todo")
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    sys.error("todo")
}

