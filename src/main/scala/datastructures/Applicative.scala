package datastructures

/**
 * All Applicatives are functors, that is why the extension
 **/
trait Applicative[F[_]] extends Functor[F] {

  // primitive combinators
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

  // derived from functor
  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))

  // () is the sole value if Unit

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]())) { (a, fbs) =>
      map2(f(a), fbs)(_ :: _)
    }

  /** Exercise 12.1 */
  def sequence[A](fas: List[F[A]]): F[List[A]] = ???

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = ???

  def product[A, B](fa: F[A], fb: F[A]): F[(A, B)] = ???

}
