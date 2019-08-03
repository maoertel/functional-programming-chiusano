package datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 3.25
  def size[A](t: Tree[A]): Int = {
    def traverse(tree: Tree[A], acc: Int): Int = tree match {
      case Leaf(_) => acc + 1
      case Branch(left, right) => traverse(left, acc) + traverse(right, acc)
    }

    traverse(t, 0)
  }

  // Exercise 3.26
  def maximum(t: Tree[Int]): Int = {
    def traverse(tree: Tree[Int], maximum: Int): Int = tree match {
      case Leaf(value) => maximum max value
      case Branch(left, right) => traverse(left, maximum) max traverse(right, maximum)
    }

    traverse(t, 0)
  }

  // Exercise 3.27
  def depth(): Int = ??? // TODO

  // Exercise 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // Exercise 3.29
  def fold[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(fold(left)(f), fold(right)(f))
  }

}
