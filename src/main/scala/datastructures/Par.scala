package datastructures

import java.util.concurrent.Callable

import scala.concurrent.Future
import scala.language.implicitConversions

trait Par[A] {

  /** Exercise 7.1 */
  def map2[B, C](other: Par[B])(f: (A, B) => C): Par[C]

}

object Par {

  /** Exercise 7.2 */
  def unit[A](a: A): Par[A] = ???

  def fork[A](a: => Par[A]): Par[A] = ???

  def lazyUnit[A](a: => A): Par[A] = ???

  def run[A](a: Par[A]): A = ???

  implicit def map[A, B, C](implicit thys: Par[A], other: Par[B]): Par[C] = ???

}

abstract class ExecutorService {

  def submit[A](a: Callable[A]): Future[A]

}