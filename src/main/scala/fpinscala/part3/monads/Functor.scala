package fpinscala.part3.monads

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(ab: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](efab: Either[F[A], F[B]]): F[Either[A, B]] =
    efab match {
      case Left(fa)  => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
}

object Functor {}
