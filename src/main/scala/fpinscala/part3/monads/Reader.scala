package fpinscala.part3.monads

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    def unit[A](a: => A): Reader[R, A] =
      Reader(_ => a)

    def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader { r =>
        val a = st.run(r)
        f(a).run(r)
      }
  }

  def read[R]: Reader[R, R] =
    Reader(r => r)
}
