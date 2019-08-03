package datastructures

/**
 * Exercise 3.1
 */
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.2
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => throw new IllegalArgumentException
    case Cons(_, Nil) => Nil
    case Cons(_, xs) => xs
  }

  // Exercise 3.3
  def setHead[A](x: A, xs: List[A]): List[A] = xs match {
    case Nil => Cons(x, Nil)
    case Cons(_, Nil) => Cons(x, Nil)
    case Cons(_, tail) => Cons(x, tail)
  }

  // Exercise 3.4
  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(_, tail) => if (n > 0) drop(tail, n - 1) else tail
  }

  // Exercise 3.5
  @scala.annotation.tailrec
  def dropWhile[A](list: List[A])(f: A => Boolean): List[A] = list match {
    case Cons(head, tail) if f(head) => dropWhile(tail)(f)
    case _ => list
  }

  def append[A](list1: List[A], list2: List[A]): List[A] = list1 match {
    case Nil => list2
    case Cons(head, tail) => Cons(head, append(tail, list2))
  }

  // Exercise 3.6
  def init[A](list: List[A]): List[A] = {
    @scala.annotation.tailrec
    def traverse(sublist: List[A], acc: List[A]): List[A] = sublist match {
      case Nil => ??? //throw IllegalArgumentException
      case Cons(_, Nil) => sublist
      case Cons(head, tail) => traverse(tail, append(acc, Cons(head, Nil)))
    }

    traverse(list, List())
  }

  // not tail recursive so it is not stack-safe
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(head, tail) => f(head, foldRight(tail, z)(f))
  }

  def sum1(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def sum(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)

  // Exercise 3.7
  @scala.annotation.tailrec
  def product(ns: List[Double]): Double = ns match {
    case Nil => 0.0
    case Cons(head, Nil) => head
    case Cons(head, tail) => if (head == 0.0) head else product(tail)
  }

  // Exercise 3.8
  // see what happens when you pass Nil and Cons...

  // Exercise 3.9
  //  def length[A](as: List[A]): Int = foldRight(as, 0) {_ + 1}

  // Exercise 3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @scala.annotation.tailrec
    def traverse(list: List[A], acc: B): B = list match {
      case Nil => ??? // f(acc, z) TODO
      case Cons(head, tail) => traverse(tail, f(acc, head))
    }

    traverse(as, z)
  }

  // Exercise 3.11
  def sum311(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product311(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  def length311[A](ns: List[A]): Int = foldLeft(ns, 0) { (x, _) => x + 1 }

  // 3.12
  def reverse[A](list: List[A]): List[A] = ??? //foldLeft(list, List())((x, y) => append(x, Cons(y, Nil))) TODO

  // 3.13


  // 3.14
  def appendWithFoldLeft[A](as: List[A], bs: List[A]): List[A] =
    foldLeft(List(as, bs), List())((x, y) => ???) // TODO

  // 3.15 TODO
  def concatList[A](list: List[List[A]]): List[A] = list match {
    case Nil => Nil
    case Cons(head, Nil) => head
    case Cons(head, tail) => head match {
      case Nil => ???
      case Cons(h, Nil) => ???
      case Cons(h, t) => ???
    }
  }

  // Exercise 3.16
  @scala.annotation.tailrec
  def plusOne(list: List[Int]): List[Int] = list match {
    case Nil => Nil
    case Cons(head, Nil) => Cons(head + 1, Nil)
    case Cons(head, tail) => plusOne(Cons(head + 1, tail))
  }

  // Exercise 3.17
  def doubleToString(list: List[Double]): List[String] = map(list)(_.toString)

  // Exercise 3.18
  def map[A, B](list: List[A])(f: A => B): List[B] = {
    @scala.annotation.tailrec
    def traverse(xs: List[A], acc: List[B]): List[B] = xs match {
      case Nil => Nil
      case Cons(head, Nil) => append(acc, List(f(head)))
      case Cons(head, tail) => traverse(tail, append(acc, List(f(head))))
    }

    traverse(list, List())
  }

  // Exercise 3.19
  def filter[A](list: List[A])(f: A => Boolean): List[A] = {
    @scala.annotation.tailrec
    def traverse(xs: List[A], acc: List[A]): List[A] = xs match {
      case Nil => Nil
      case Cons(head, tail) =>
        if (f(head)) traverse(tail, acc match { case Nil => Nil; case Cons(_, tail) => tail })
        else traverse(tail, acc)
    }

    traverse(list, List())
  }

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    @scala.annotation.tailrec
    def traverse(xs: List[A], acc: List[B]): List[B] = xs match {
      case Nil => acc
      case Cons(head, tail) => traverse(tail, append(acc, f(head)))
    }

    traverse(as, List())
  }

  // Exercise 3.21
  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((x: A) => if (f(x)) Nil else List(x))

  // Exercise 3.22
  def corresponding(as: List[Int], bs: List[Int]): List[Int] = {
    @scala.annotation.tailrec
    def traverse(a: List[Int], b: List[Int], acc: List[Int]): List[Int] = a match {
      case Nil => Nil
      case Cons(aHead, Nil) => b match {
        case Cons(bHead, Nil) => append(acc, List(aHead + bHead))
      }
      case Cons(aHead, aTail) => b match {
        case Cons(bHead, bTail) => traverse(aTail, bTail, append(acc, List(aHead + bHead)))
      }
    }

    traverse(as, bs, List())
  }

  // Exercise 3.23
  def zipWith[A](as: List[A], bs: List[A])(f: (A, A) => A): List[A] = {
    @scala.annotation.tailrec
    def traverse(a: List[A], b: List[A], acc: List[A]): List[A] = a match {
      case Nil => Nil
      case Cons(aHead, Nil) => b match {
        case Cons(bHead, Nil) => append(acc, List(f(aHead, bHead)))
      }
      case Cons(aHead, aTail) => b match {
        case Cons(bHead, bTail) => traverse(aTail, bTail, append(acc, List(f(aHead, bHead))))
      }
    }

    traverse(as, bs, List())
  }

}

object Main extends App {

  val list = List(1, 2, 3)
  List.reverse(list)

}