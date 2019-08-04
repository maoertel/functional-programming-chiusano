package datastructures

/** Exercise 4.6 */
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(value) => Left(value)
    case Right(value) => Right(f(value))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(value) => Left(value)
    case Right(value) => f(value)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(value) => b
    case Right(value) => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case Left(value) => Left(value)
    case Right(valueA) => b match {
      case Left(value) => Left(value)
      case Right(valueB) => Right(f(valueA, valueB))
    }
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  /** Exercise 4.7 – part 1 */
  def sequence[E, A](es: MyList[Either[E, A]]): Either[E, MyList[A]] = {

    @scala.annotation.tailrec
    def traverse(list: MyList[Either[E, A]], acc: MyList[A]): Either[E, MyList[A]] = list match {
      case Nil => Right(Nil)
      case Cons(head, Nil) => head match {
        case Left(value) => Left(value)
        case Right(value) => Right(MyList.append(acc, MyList(value)))
      }
      case Cons(head, tail) => head match {
        case Left(value) => Left(value)
        case Right(value) => traverse(tail, MyList.append(acc, MyList(value)))
      }
    }

    traverse(es, MyList())
  }

  /** Exercise 4.7 – part 2 */
  def traverse[E, A, B](as: MyList[A])(f: A => Either[E, B]): Either[E, MyList[B]] = {

    @scala.annotation.tailrec
    def inner(list: MyList[A], acc: MyList[B]): Either[E, MyList[B]] = list match {
      case Nil => Right(Nil)
      case Cons(head, Nil) => f(head) match {
        case Left(value) => Left(value)
        case Right(value) => Right(MyList.append(acc, MyList(value)))
      }
      case Cons(head, tail) => f(head) match {
        case Left(value) => Left(value)
        case Right(value) => inner(tail, MyList.append(acc, MyList(value)))
      }
    }

    inner(as, MyList())
  }

}