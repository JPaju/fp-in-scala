package part1.laziness

import Stream._

import scala.annotation.tailrec

sealed trait Stream[+A] {
	def toList: List[A] = this match {
		case Empty => List()
		case Cons(h, t) => h() :: t().toList
	}

	def headOption: Option[A] = this match {
		case Empty => None
		case Cons(h, _) => Some(h())
	}

	def take(n: Int): Stream[A] = this match {
		case Cons(h, _) if n == 1 => cons(h(), empty)
		case Cons(h, t) if n > 0 => cons(h(), t().take(n-1))
		case _ => Empty
	}

	@tailrec
	final def drop(n: Int): Stream[A] = this match {
		case Cons(_, t) if n > 0 => t().drop(n-1)
		case Cons(h, t) => cons(h(), t())
		case Empty => Empty
	}

	def takeWhile(p: A => Boolean): Stream[A] = this match {
		case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
		case _ => Empty
	}

	def foldRight[B](z: => B)(f: (A, => B) => B): B =
		this match {
			case Cons(h, t) => f(h(), t().foldRight(z)(f))
			case _ => z
		}

	def forAll(p: A => Boolean): Boolean =
		foldRight(true)((h,t) => p(h) && t)

	def takeWhile2(p: A => Boolean): Stream[A] =
		foldRight(empty[A])((h,t) => if(p(h)) cons(h, t) else empty)

	def map[B](f: A => B): Stream[B] =
		foldRight(empty[B])((h,t) => cons(f(h), t))

	def filter(p: A => Boolean): Stream[A] =
		foldRight(empty[A])((h,t) => if (p(h)) cons(h, t) else t)

	def append[B >: A](e: => Stream[B]): Stream[B] =
		foldRight(e)(cons(_, _))

	def flatMap[B](f: A => Stream[B]): Stream[B] =
		foldRight(empty[B])((h,t) => f(h) append t)

	def mapViaUnfold[B](f: A => B): Stream[B] =
		unfold(this) {
			case Cons(h,t) => Some(f(h()), t())
			case Empty => None
		}

	def takeViaUnfold(n: Int): Stream[A] =
		unfold((this, n)) {
			case (Cons(h,t), n) if n > 0 => Some(h(), (t(), n-1))
			case _ => None
		}

	def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
		unfold(this) {
			case Cons(h, t) if p(h()) => Some(h(), t())
			case _ => None
		}

	def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
		unfold((this, s2)){
			case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
			case _ => None
		}

	def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
		unfold((this, s2)) {
			case (Empty, Cons(h,t)) => Some((None, Some(h())), (empty, t()))
			case (Cons(h,t), Empty) => Some((Some(h()), None), (t(), empty))
			case (Cons(h1,t1), Cons(h2,t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
			case _ => None
		}

	def startsWith[B](s: Stream[B]): Boolean =
		zipAll(s).takeWhile(_._2.isDefined) forAll { case (a,b) => a == b}

	def tails: Stream[Stream[A]] =
		unfold(this) {
			case Cons(h,t) => Some((cons(h(), t()), t()))
			case Empty => None
		}

	def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
		foldRight((z, Stream(z)))((a, b0) => {
			lazy val b1 = b0
			val b2 = f(a, b1._1)
			(b2, cons(b2, b1._2))
		})._2
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
	def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)
	}

	def empty[A]: Stream[A] = Empty

	def apply[A](as: A*): Stream[A] =
		if (as.isEmpty) empty
		else cons(as.head, apply(as.tail: _*))

	def constant[A](a: A): Stream[A] = cons(a, constant(a))

	def from(n: Int): Stream[Int] = cons(n, from(n+1))

	def fibs: Stream[Int] = {
		def go(curr: Int, next: Int): Stream[Int] =
			cons(curr, go(next, curr+next))
		go(0, 1)
	}

	def unfold[A,S](z: S)(f: S => Option[(A, S)]): Stream[A] =
		f(z) match {
			case Some((a, s)) => cons(a, unfold(s)(f))
			case None => empty[A]
		}

	def constantViaUnfold[A](a: A): Stream[A] =
		unfold(a)(_ => Some(a, a))

	def fromViaUnfold(n: Int): Stream[Int] =
		unfold(n)(n => Some(n, n+1))

	def fibsViaUnfold: Stream[Int] =
		unfold((0,1)){ case (curr, next) => Some(curr, (next, curr+next))}

}