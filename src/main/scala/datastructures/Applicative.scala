package datastructures

/**
 * All Applicatives are functors, that is why the extension of Functor[F].
 * So all Monads are applicative Functors.
 *
 * Laws:
 * I. Left & right identity laws from Functor
 * - map(v)(id) == v
 * - map(map(v)(g))(f) == map(v)(f compose g)
 *
 * II. Associativity law
 * - product(product(fa, fb), fc) == map(fa, product(fb, fc)))(assoc)
 *
 **/
trait Applicative[F[_]] extends Functor[F] {

  /** Exercise 12.2 – apply in terms of map2 & unit */
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = ???

  // primitive combinators
  /** let us add a function within F */
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  /** adds an layer of F */
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

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def assoc[A, B, C](p: (A, (B, C))): ((A, B), C) = p match {
    case (a, (b, c)) => ((a, b), c)
  }
}

/**
 * A minimal implementation of Monad must implement unit and override either flatMap or join & map
 **/
private trait Monad2[F[_]] extends Applicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  /** removes an layer of F */
  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))

}

/**
 * Definition: effects
 *
 * - informally type constructors like Option, List, a.s.o. are called effects. These types are called effects because
 * they augment ordinary values with extra capabilities.
 *
 * Definition: monadic or applicative effects
 *
 * - to mean types with an associated Monad or Applicative instance
 */