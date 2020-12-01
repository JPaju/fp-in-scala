package fpinscala.part1.errorhandling


import scala.{Option => _, Either => _, _}

sealed trait Either[+E, +A] {

	def map[B](f: A => B): Either[E, B] = this match {
		case Left(v) =>  Left(v)
		case Right(v) => Right(f(v))
	}

	def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
		this match {
			case Left(v) => Left(v)
			case Right(v) => f(v)
		}

	def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
		this match {
			case Left(_) => b
			case Right(v) => Right(v)
		}

	def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
		for {
			av <- this
			bv <- b
		} yield f(av, bv)
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


object Either {
	def sequence[E, A](l: List[Either[E, A]]): Either[E, List[A]] =
		traverse(l)(identity)

	def traverse[E, A, B](l: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
		l match {
			case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
			case Nil => Right(Nil)
		}
}

