package datastructures

import scala.language.higherKinds

/**
 * A monads minimal sets are are:
 *  - unit & flatMap
 *  - unit & compose
 *  - unit, map, join
 *
 * A monad satisfies the laws of identity (left/right unit) and associativity and implements one of the minimal sets.
 *
 * I. Associativity law:
 *
 * (x.flatMap(f)).flatMap(g) == x.flatMap(a => f(a).flatMap(g)) --> should be the same in the end
 *
 * II. Identity laws: left unit & right unit law
 *
 * Right unit law
 *
 * - either: compose(f, unit) == f
 *
 * - or: flatMap(x)(unit) == x
 *
 * Left unit law
 *
 * - either: compose(unit,f) == f
 *
 * - or: flatMap(unit(y))(f) == f(y)
 *
 *
 * --> Kleisli arrows are functions in the form of: A => F[A]
 **/
trait Monad[F[_]] {

  def unit[A](a: => A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))

  /** Exercise 11.3 */
  def sequence[A](lma: List[F[A]]): F[List[A]] = unit(lma.iterator.map(a => map(a)(_)))

  /** sequence in terms of traverse */
  def sequence2[A](lfa: List[F[A]]): F[List[A]] = traverse(lfa)(fa => fa)

  /** Exercise 11.3 */
  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la.iterator.map(a => f(a)).toList)

  /** Exercise  11.4 */
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = ??? // TODO

  /** Exercise 11.5 */
  // TODO description of replicateM in terms of List, Option a.s.o.

  /** Exercise 11.6 */
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = sequence(ms map f toList)

  /** Exercise 11.7 – compose in terms of Kleisli arrows */
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = { m: A =>
    val afterF: F[B] = flatMap(unit(m))(f(_))
    flatMap(afterF)(g(_))
  }

  /** Exercise 11.12 */
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(fa => fa)

}

object Monad {

  /** Exercise 11.1 */
  val listMonad: Monad[List] = new Monad[List[Int]] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa flatMap f
  }

  /** Exercise 11.1 */
  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa flatMap f
  }

  /** Exercise 11.1 */
  val streamMonad: Monad[MyStream] = new Monad[MyStream] {
    override def unit[A](a: => A): MyStream[A] = MyStream(a)

    override def flatMap[A, B](fa: MyStream[A])(f: A => MyStream[B]): MyStream[B] = fa flatMap f
  }

}