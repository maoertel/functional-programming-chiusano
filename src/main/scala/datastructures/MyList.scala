package datastructures

/**
 * Exercise 3.1
 */
sealed trait MyList[+A]

case object Nil extends MyList[Nothing]

case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.2
  def tail[A](xs: MyList[A]): MyList[A] = xs match {
    case Nil => throw new IllegalArgumentException
    case Cons(_, Nil) => Nil
    case Cons(_, xs) => xs
  }

  // Exercise 3.3
  def setHead[A](x: A, xs: MyList[A]): MyList[A] = xs match {
    case Nil => Cons(x, Nil)
    case Cons(_, Nil) => Cons(x, Nil)
    case Cons(_, tail) => Cons(x, tail)
  }

  // Exercise 3.4
  @scala.annotation.tailrec
  def drop[A](l: MyList[A], n: Int): MyList[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(_, tail) => if (n > 0) drop(tail, n - 1) else tail
  }

  // Exercise 3.5
  @scala.annotation.tailrec
  def dropWhile[A](list: MyList[A])(f: A => Boolean): MyList[A] = list match {
    case Cons(head, tail) if f(head) => dropWhile(tail)(f)
    case _ => list
  }

  def append[A](list1: MyList[A], list2: MyList[A]): MyList[A] = list1 match {
    case Nil => list2
    case Cons(head, tail) => Cons(head, append(tail, list2))
  }

  // Exercise 3.6
  def init[A](list: MyList[A]): MyList[A] = {
    @scala.annotation.tailrec
    def traverse(sublist: MyList[A], acc: MyList[A]): MyList[A] = sublist match {
      case Nil => Nil
      case Cons(_, Nil) => sublist
      case Cons(head, tail) => traverse(tail, append(acc, Cons(head, Nil)))
    }

    traverse(list, MyList())
  }

  // not tail recursive so it is not stack-safe
  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(head, tail) => f(head, foldRight(tail, z)(f))
  }

  def sum1(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def sum(ns: MyList[Int]): Int = foldRight(ns, 0)(_ + _)

  // Exercise 3.7
  @scala.annotation.tailrec
  def product(ns: MyList[Double]): Double = ns match {
    case Nil => 0.0
    case Cons(head, Nil) => head
    case Cons(head, tail) => if (head == 0.0) head else product(tail)
  }

  // Exercise 3.8
  // see what happens when you pass Nil and Cons...

  // Exercise 3.9
  //  def length[A](as: List[A]): Int = foldRight(as, 0) {_ + 1}

  // Exercise 3.10
  def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = {
    @scala.annotation.tailrec
    def traverse(list: MyList[A], acc: B): B = list match {
      case Nil => acc
      case Cons(head, tail) => traverse(tail, f(acc, head))
    }

    traverse(as, z)
  }

  // Exercise 3.11
  def sum311(ns: MyList[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product311(ns: MyList[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  def length311[A](ns: MyList[A]): Int = foldLeft(ns, 0) { (x, _) => x + 1 }

  // 3.12 TODO
  def reverse[A](list: MyList[A]): MyList[A] = ??? //foldLeft(list, List())((x, y) => append(x, Cons(y, Nil)))

  // 3.13 TODO

  // 3.14 TODO
  def appendWithFoldLeft[A](as: MyList[A], bs: MyList[A]): MyList[A] =
    foldLeft(MyList(as, bs), MyList())((x, y) => ???)

  // 3.15 TODO
  def concatList[A](list: MyList[MyList[A]]): MyList[A] = list match {
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
  def plusOne(list: MyList[Int]): MyList[Int] = list match {
    case Nil => Nil
    case Cons(head, Nil) => Cons(head + 1, Nil)
    case Cons(head, tail) => plusOne(Cons(head + 1, tail))
  }

  // Exercise 3.17
  def doubleToString(list: MyList[Double]): MyList[String] = map(list)(_.toString)

  // Exercise 3.18
  def map[A, B](list: MyList[A])(f: A => B): MyList[B] = {
    @scala.annotation.tailrec
    def traverse(xs: MyList[A], acc: MyList[B]): MyList[B] = xs match {
      case Nil => Nil
      case Cons(head, Nil) => append(acc, MyList(f(head)))
      case Cons(head, tail) => traverse(tail, append(acc, MyList(f(head))))
    }

    traverse(list, MyList())
  }

  // Exercise 3.19
  def filter[A](list: MyList[A])(f: A => Boolean): MyList[A] = {
    @scala.annotation.tailrec
    def traverse(xs: MyList[A], acc: MyList[A]): MyList[A] = xs match {
      case Nil => Nil
      case Cons(head, tail) =>
        if (f(head)) traverse(tail, acc match { case Nil => Nil; case Cons(_, tail) => tail })
        else traverse(tail, acc)
    }

    traverse(list, MyList())
  }

  // Exercise 3.20
  def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] = {
    @scala.annotation.tailrec
    def traverse(xs: MyList[A], acc: MyList[B]): MyList[B] = xs match {
      case Nil => acc
      case Cons(head, tail) => traverse(tail, append(acc, f(head)))
    }

    traverse(as, MyList())
  }

  // Exercise 3.21
  def filterWithFlatMap[A](as: MyList[A])(f: A => Boolean): MyList[A] =
    flatMap(as)((x: A) => if (f(x)) Nil else MyList(x))

  // Exercise 3.22
  def corresponding(as: MyList[Int], bs: MyList[Int]): MyList[Int] = {
    @scala.annotation.tailrec
    def traverse(a: MyList[Int], b: MyList[Int], acc: MyList[Int]): MyList[Int] = a match {
      case Nil => Nil
      case Cons(aHead, Nil) => b match {
        case Cons(bHead, Nil) => append(acc, MyList(aHead + bHead))
      }
      case Cons(aHead, aTail) => b match {
        case Cons(bHead, bTail) => traverse(aTail, bTail, append(acc, MyList(aHead + bHead)))
      }
    }

    traverse(as, bs, MyList())
  }

  // Exercise 3.23
  def zipWith[A](as: MyList[A], bs: MyList[A])(f: (A, A) => A): MyList[A] = {
    @scala.annotation.tailrec
    def traverse(a: MyList[A], b: MyList[A], acc: MyList[A]): MyList[A] = a match {
      case Nil => Nil
      case Cons(aHead, Nil) => b match {
        case Cons(bHead, Nil) => append(acc, MyList(f(aHead, bHead)))
      }
      case Cons(aHead, aTail) => b match {
        case Cons(bHead, bTail) => traverse(aTail, bTail, append(acc, MyList(f(aHead, bHead))))
      }
    }

    traverse(as, bs, MyList())
  }

}

object Main extends App {

  val list = MyList(1, 2, 3)
  println(MyList.append(list, list))
  println(MyList.zipWith(list, list)(_ + _))

}