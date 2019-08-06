package datastructures

import datastructures.MyStream.{cons, empty}

sealed trait MyStream[+A] {

  /** Exercise 5.1 */
  def toList: List[A] = {
    @scala.annotation.tailrec
    def traverse(stream: MyStream[A], acc: List[A]): List[A] = stream match {
      case Const(h, t) => traverse(t(), acc ::: List(h()))
      case _ => List()
    }

    traverse(this, List())
  }

  /** Exercise 5.2 – part 1 */
  def take(n: Int): MyStream[A] = this match {
    case Empty => empty
    case Const(h, t) =>
      if (n > 0) cons(h(), t().take(n - 1))
      else empty
  }

  /** Exercise 5.2 – part 2 */
  def drop(n: Int): MyStream[A] = {

    @scala.annotation.tailrec
    def traverse(stream: MyStream[A], n: Int): MyStream[A] = stream match {
      case Const(_, tail) => if (n > 0) traverse(tail(), n - 1) else tail()
      case _ => this
    }

    traverse(this, n)
  }

  /** Exercise 5.3 */
  def takeWhile(predicate: A => Boolean): MyStream[A] = this match {
    case Empty => empty
    case Const(head, tail) =>
      if (predicate(head()))
        cons(head(), tail().takeWhile(predicate))
      else
        empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Const(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def exists2(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Const(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  /** Exercise 5.4 */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => if (p(a)) b else false)

  /** Exercise 5.5. */
  def takeWhile2(p: A => Boolean): MyStream[A] = foldRight(this)((a, b) => if (p(a)) this else b)

  /** Exercise 5.6 */
  // TODO

  /** Exercise 5.7 */
  def map[B](f: A => B): MyStream[B] = foldRight(empty)((a, b) => cons(f(a), b.map(f)))

  def filter(p: A => Boolean): MyStream[A] = foldRight(empty) { (a, b) =>
    if (p(a)) b.filter(p)
    else cons(a, b.filter(p))
  }

  def append[A](as: MyStream[A]): MyStream[A] = foldRight(as)((a, b) => cons(a, append(b)))

  def flatMap[B](f: A => MyStream[B]): MyStream[B] = foldRight(empty)((a, b) => cons(f(a), b.flatMap(f)))

  //  def find(p: A => Boolean): Option[A] = filter(p).headOption // TODO implementation of headOption missing

  def ones: MyStream[Int] = MyStream.cons(1, ones)

}

case object Empty extends MyStream[Nothing]

case class Const[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

object MyStream {

  /** Exercise 5.8 */
  def constant[A](a: A): MyStream[A] = cons(a, constant(a))

  /** Exercise 5.9 */
  def from(n: Int): MyStream[Int] = {
    def traverse(count: Int): MyStream[Int] =
      if (count == n) empty
      else cons(count, traverse(count + 1))

    traverse(1)
  }

  /** Exercise 5.10 */
  def fibs: MyStream[Int] = {
    def inner(prior: Int, current: Int): MyStream[Int] = cons(prior, inner(current, prior + current))

    inner(0, 1)
  }

  /** Exercise 5.11 */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => Empty
  }

  /** TODO Exercises 5.12 to 5.16 are missing */

  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Const(() => head, () => tail)
  }

  def empty[A]: MyStream[A] = Empty

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

}