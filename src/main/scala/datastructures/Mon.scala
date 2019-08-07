package datastructures

trait Mon[F[_]] {

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))

}

object Main {

  val listMonad: Mon[List] = new Mon[List[Int]] {

    override def map[A, B](fa: List[A])(f: A => B): List[B] = ???

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = ???
  }

}