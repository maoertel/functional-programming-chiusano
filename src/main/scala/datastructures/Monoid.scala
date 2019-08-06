package datastructures

trait Monoid[A] {

  def op(a1: A, a2: A): A

  def zero: A

}


object Test {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  val listMonoid: Monoid[List[_]] = new Monoid[List[_]] {
    override def op(a1: List[_], a2: List[_]): List[_] = a1 ++ a2

    override def zero: List[_] = List()
  }

  /** Exercise 10.1 */
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = ??? // TODO
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = // TODO
  }

  /** Exercise 10.2 */
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 match {
      case Some(get1) => a2 match {
        case Some(get2) => Some(get1 + get2)
        case None => None
      }
      case None => None
    }

    override def zero: Option[A] = ??? // TODO
  }

  /** Exercise 10.3 */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = ??? // TODO

    override def zero: A => A = ??? // TODO
  }

  // That is amazing
  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  /** Exercise 10.5 */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.map(f(_)).foldLeft(m.zero)(m.op)

  /** Exercise 10.6 */
  def foldRight[A](as: List[A], m: Monoid[A])(f: (A, A) => A): A = ??? // TODO

}
