package introduction

sealed trait Tree[+A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]

object Tree {

	def size[A](t: Tree[A]): Int = t match {
		case Branch(l, r) => 1 + size(l) + size(r)
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

	def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
		case Branch(l, r) => Branch(map(l)(f), map(r)(f))
		case Leaf(v) => Leaf(f(v))
	}

	def fold[A,B](t: Tree[A])(f: A => B)(combine: (B,B) => B): B = t match {
		case Branch(l, r) => combine(fold(l)(f)(combine), fold(r)(f)(combine))
		case Leaf(v) => f(v)
	}

	def size2[A](t: Tree[A]): Int =
		fold(t)(_ => 1)(1 + _ + _)

	def maximum2(t: Tree[Int]): Int =
		fold(t)(identity)(_ max _)

	def depth2[A](t: Tree[A]): Int =
		fold(t)(_ => 1)((ld,rd) => 1 + (ld max rd))

	def map2[A,B](t: Tree[A])(f: A => B): Tree[B] =
		fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_,_))
}