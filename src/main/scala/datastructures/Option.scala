package datastructures

/** Exercise 4.1 – implement all functions from Options trait */
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(value) => Some(f(value))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(value) => f(value)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(value) => value
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(_) => this
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(value) => if (f(value)) None else this
    case None => None
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Main extends App {

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] = ??? // TODO

  // Exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a match {
    case None => None
    case Some(getA) => b match {
      case Some(getB) => Some(f(getA, getB))
      case None => None
    }
  }

  def sequence[A](list: List[Option[A]]): Option[List[A]] =
    Some(List.flatMap(list) { case Some(value) => List(value) })

}