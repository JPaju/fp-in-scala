package fpinscala.part3.monads

import scala.language.higherKinds

import fpinscala.part1.laziness.Stream
import fpinscala.part1.state.State
import fpinscala.part2.parallelism.Par
import fpinscala.part2.parallelism.Par.Par
import fpinscala.part2.testing.Gen

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](fa: F[A])(afb: A => F[B]): F[B]

  def map[A, B](fa: F[A])(ab: A => B): F[B] =
    flatMap(fa)(a => unit(ab(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(abc: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => abc(a, b)))

  def sequence[A](lfa: List[F[A]]): F[List[A]] =
    lfa.foldRight(unit(List[A]()))((fa, fla) => map2(fa, fla)(_ :: _))

  def traverse[A, B](la: List[A])(afb: A => F[B]): F[List[B]] =
    sequence(la.map(afb))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as.foldRight(unit(List[A]()))((a, fla) => map2(fla, f(a))((la, keep) => if (keep) a :: la else la))

  def compose[A, B, C](afb: A => F[B], bfc: B => F[C]): A => F[C] =
    a => flatMap(afb(a))(bfc)

  def flatMapViaCompose[A, B](fa: F[A])(afb: A => F[B]): F[B] =
    compose((_: Unit) => fa, afb)(())

  def join[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(identity)

  def flatMapViaJoin[A, B](fa: F[A])(afb: A => F[B]): F[B] =
    join(map(fa)(afb))

  def composeViaJoin[A, B, C](afb: A => F[B], bfc: B => F[C]): A => F[C] =
    a => join(map(afb(a))(b => bfc(b)))
}

object Monad {
  val genMonad = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] =
      Gen.unit(a)

    override def flatMap[A, B](fa: Gen[A])(afb: A => Gen[B]): Gen[B] =
      fa.flatMap(afb)
  }

  val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] =
      Par.unit(a)

    override def flatMap[A, B](fa: Par[A])(afb: A => Par[B]): Par[B] =
      Par.flatMap(fa)(afb)
  }

  val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] =
      Some(a)

    override def flatMap[A, B](fa: Option[A])(afb: A => Option[B]): Option[B] =
      fa.flatMap(afb)
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] =
      Stream(a)

    override def flatMap[A, B](fa: Stream[A])(afb: A => Stream[B]): Stream[B] =
      fa.flatMap(afb)
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] =
      List(a)

    override def flatMap[A, B](fa: List[A])(afb: A => List[B]): List[B] =
      fa.flatMap(afb)
  }

  def stateMonad[S]: Monad[({ type AState[a] = State[S, a] })#AState] =
    new Monad[({ type AState[a] = State[S, a] })#AState] {
      override def unit[A](a: => A): State[S, A] =
        State.unit(a)

      override def flatMap[A, B](fa: State[S, A])(afb: A => State[S, B]): State[S, B] =
        fa.flatMap(afb)
    }
}
