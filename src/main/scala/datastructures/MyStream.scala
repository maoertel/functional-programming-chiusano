package datastructures

sealed trait MyStream[+A] {

  def tolist: List[A] = {
    @scala.annotation.tailrec
    def traverse(stream: MyStream[A], acc: List[A]): List[A] = stream match {
      case Empty => List()
      case Const(h, t) => traverse(t(), acc :: List(h()))
    }

    traverse(this, List())
  }

}

case object Empty extends MyStream[Nothing]

case class Const[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

object MyStream {

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
