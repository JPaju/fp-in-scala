package fpinscala.part3.monads

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {
  val monad = new Monad[Id] {
    override def unit[A](a: => A) = Id(a)

    override def flatMap[A, B](fa: Id[A])(afb: A => Id[B]) = fa.flatMap(afb)
  }
}

object ZeroOverheadId {
  type ZeroOverheadId[A] = A

  def apply[A](a: A): ZeroOverheadId[A] =
    a

  implicit val idMonad: Monad[ZeroOverheadId] = new Monad[ZeroOverheadId] {
    override def unit[A](a: => A): ZeroOverheadId[A] = a

    override def flatMap[A, B](fa: ZeroOverheadId[A])(afb: A => ZeroOverheadId[B]): ZeroOverheadId[B] =
      afb(fa)
  }
}
