package fpinscala.part3

import fpinscala.part2.testing.Prop.forAll
import fpinscala.part2.testing.{Gen, Prop}

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

	def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
		val associativity = forAll(for {
			a <- gen
			b <- gen
			c <- gen
		} yield (a, b, c)) { case(a, b, c) =>
			m.op(a, m.op(b, c)) == m.op(m.op(a, b), c)
		}

		val identity = forAll(gen) { a =>
			(m.op(a, m.zero) == a) &&
		  	(m.op(m.zero, a) == a)
		}

		associativity && identity
	}
}
