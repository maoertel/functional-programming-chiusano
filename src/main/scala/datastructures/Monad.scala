package datastructures

/**
 * Monad with unit and flatMap as minimal set
 */
trait Monad[F[_]] {

  def unit[A](a: => A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))

  /** Exercise 11.3 */
  def sequence[A](lma: List[F[A]]): F[List[A]] = unit(lma.iterator.map(a => map(a)(_)))

  /** Exercise 11.3 */
  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la.iterator.map(a => f(a)).toList)

}

object Monad {

  /** Exercise 11.1 */
  val listMonad: Monad[List] = new Monad[List[Int]] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
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