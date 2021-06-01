package fpinscala.part3.monoids

import fpinscala.part2.testing.{ Gen, Prop }
import fpinscala.part2.testing.Prop.forAll

trait Monoid[A] {
  def op(a: A, b: A): A

  def zero: A
}

object Monoid {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a: String, b: String) = a + b

    val zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a: List[A], b: List[A]) = a ++ b

    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int) = x + y

    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(x: Int, y: Int) = x * y

    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x || y

    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x && y

    val zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a: Option[A], b: Option[A]) = a orElse b

    val zero = Option.empty
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a: A => A, b: A => A) = a andThen b

    val zero = identity
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a: A, b: A): A = m.op(b, a)

    val zero = m.zero
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val associativity = forAll(for {
      a <- gen
      b <- gen
      c <- gen
    } yield (a, b, c)) {
      case (a, b, c) =>
        m.op(a, m.op(b, c)) == m.op(m.op(a, b), c)
    }

    val identity = forAll(gen) { a =>
      (m.op(a, m.zero) == a) &&
      (m.op(m.zero, a) == a)
    }

    associativity && identity
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldLeft[X, Y](xs: List[X])(z: Y)(f: (X, Y) => Y): Y =
    foldMap(xs, dual(endoMonoid[Y]))(f.curried)(z)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v match {
    case IndexedSeq()  => m.zero
    case IndexedSeq(a) => f(a)
    case _ =>
      val (left, right) = v.splitAt(v.length / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
  }

  def productMonoid[A, B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a: (A, B), b: (A, B)) =
      (ma.op(a._1, b._1), mb.op(a._2, b._2))

    override val zero = (ma.zero, mb.zero)
  }

  def functionMonoid[A, B](mb: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a: A => B, b: A => B) =
      (aa: A) => mb.op(a(aa), b(aa))

    override val zero = _ => mb.zero
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()

      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
        }

    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))

}
