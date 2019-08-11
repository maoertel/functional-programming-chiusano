package datastructures

/**
 * A Functor is generalizing map()
 *
 * Law: mapping over a structure with identity function should itself be an identity
 * - map(x)(a => a) == x
 *
 * - map(x) preserves the structure of x
 */
trait Functor[F[_]] {

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2)) // it is like .unzip()

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = ??? //    e match {
  //      case Left(value) => map(value)(Left(_))
  //      case Right(value) => map(value)(Right[_])
  //    }

}

/**
 * Just to remember: A type constructor like List is a Functor.
 * The Functor[F] instance constitutes prove that F is a functor.
 */