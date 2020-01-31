package introduction

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

	def apply[A](as: A*): List[A] =
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))

	def tail[A](l: List[A]): List[A] =
		l match {
			case Cons(h, t) => t
			case _ => Nil
		}

	def append[A](a1: List[A], a2: List[A]): List[A] =
		a1 match {
			case Nil => a2
			case Cons(h,t) => Cons(h, append(t, a2))
		}

	def setHead[A](l: List[A], a: A): List[A] =
		l match {
			case Cons(h, t) => Cons(a, t)
			case _ => Cons(a, Nil)
		}

	@tailrec
	def drop[A](l: List[A], n: Int): List[A] =
		if (n <= 0) l
		else l match {
			case Cons(_, tail) => drop(tail, n-1)
			case _ => Nil
		}

	@tailrec
	def dropWhile[A](l: List[A])( f: A => Boolean): List[A] =
		l match {
			case Cons(h, t) =>
				if (f(h)) dropWhile(t)(f)
				else Cons(h, t)
			case _ => Nil
		}

	def init[A](l: List[A]): List[A] =
		l match {
			case Nil => Nil
			case Cons(_, Nil) => Nil
			case Cons(h, t) => Cons(h, init(t))
		}

	def foldRight[A,B](l: List[A], id: B)( f: (A, B) => B): B =
		l match {
			case Nil => id
			case Cons(h, t) => f(h, foldRight(t, id)(f))
		}

	def sum(l: List[Int]): Int =
		foldRight(l, 0)(_ + _)

	def product(l: List[Double]): Double =
		foldRight(l, 1.0)(_ * _)

	def length[A](l: List[A]): Int =
		foldRight(l, 0)((_, x) => 1 + x)


	@tailrec
	def foldLeft[A,B](l: List[A], id: B)(f: (B, A) => B): B =
		l match {
			case Nil => id
			case Cons(h, t) => foldLeft(t, f(id, h))(f)
		}

	def sum2(l: List[Int]): Int =
		foldLeft(l, 0)(_ + _)

	def product2(l: List[Double]): Double =
		foldLeft(l, 1.0)(_ * _)

	def length2[A](l: List[A]): Int =
		foldLeft(l, 0)((acc, _) => acc + 1)

	def reverse[A](l: List[A]): List[A] =
		foldLeft(l, Nil: List[A])((head, tail) => Cons(tail, head))

	def append2[A](l1: List[A], l2: List[A]): List[A] =
		foldRight(l1, l2)(Cons(_, _))

	def flatten[A](nl: List[List[A]]): List[A] =
		foldLeft(nl, Nil: List[A])(append2)

	def add1(l: List[Int]): List[Int] =
		l match {
			case Nil => Nil
			case Cons(h, t) => Cons(h + 1, add1(t))
		}

	def doubleToString(l: List[Double]): List[String] =
		l match {
			case Nil => Nil
			case Cons(h, t) => Cons(h.toString, doubleToString(t))
		}

	def map[A, B](l: List[A])(f: A => B): List[B] =
		l match {
			case Nil => Nil
			case Cons(h, t) => Cons(f(h), map(t)(f))
		}

	def map2[A,B](l: List[A])(f: A => B): List[B] =
		foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

	def filter[A](l: List[A])(p: A => Boolean): List[A] =
		l match {
			case Nil => Nil
			case Cons(head, tail) =>
				if (p(head)) Cons(head, filter(tail)(p))
				else filter(tail)(p)
		}

	def filter2[A](l: List[A])(p: A => Boolean): List[A] =
		foldRight(l, Nil: List[A])((h, t) => if (p(h)) Cons(h, t) else t)

	def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
		flatten(map(l)(f))

	def filter3[A](l: List[A])(p: A => Boolean): List[A] =
		flatMap(l)(a => if (p(a)) List(a) else Nil)

	def zipInts(l1: List[Int], l2: List[Int]): List[Int] =
		(l1, l2) match {
			case (Cons(h1, t1), Cons(h2, t2)) =>
				Cons(h1 + h2, zipInts(t1, t2))
			case _ => Nil
		}

	def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B) => C): List[C] =
		(l1, l2) match {
			case (Cons(h1, t1), Cons(h2, t2)) =>
				Cons(f(h1,h2), zipWith(t1, t2)(f))
			case _ => Nil
		}
}
