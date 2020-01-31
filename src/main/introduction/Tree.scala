package introduction

sealed trait Tree[+A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

object Tree {

	def size[A](t: Tree[A]): Int = t match {
		case Branch(left, right) => 1 + size(left) + size(right)
		case Leaf(_) => 1
	}

	def maximum(t: Tree[Int]): Int = t match {
		case Branch(l, r) => maximum(l) max maximum(r)
		case Leaf(v) => v
	}

	def depth[A](t: Tree[A]): Int = t match {
		case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
		case Leaf(_) => 1
	}

	def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
		case Branch(l, r) => Branch(map(l)(f), map(r)(f))
		case Leaf(value) => Leaf(f(value))
	}

	def fold[A,B](t: Tree[A], id: B)(f: (A,B) => B): Tree[B] = ???
}