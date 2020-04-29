package part1.errorhandling

sealed trait Option[+A] {
	def map[B](f: A => B): Option[B] = this match {
		case Some(v) => Some(f(v))
		case None => None
	}

	def flatMap[B](f: A => Option[B]): Option[B] = this match {
		case Some(v) => f(v)
		case None => None
	}

	def getOrElse[B >: A](default: => B): B = this match {
		case Some(v) => v
		case None => default
	}

	def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
		case None => ob
		case _ => this
	}

	def filter(p: A => Boolean): Option[A] = this match {
		case Some(v) if p(v) => this
		case None => None
	}
}

case class Some[+A](v: A) extends Option[A]
case object None extends Option[Nothing]


object Option {

	def mean(s: Seq[Double]): Option[Double] =
		if (s.isEmpty) None
		else Some(s.sum / s.length)

	def variance(xs: Seq[Double]): Option[Double] = {
		mean(xs).flatMap(m => mean(xs.map(s => math.pow(s-m, 2))))
	}

	def map2[A,B,C](oa: Option[A], ob: Option[B])(f: (A,B) => C): Option[C] =
		oa.flatMap(a => ob.map(b => f(a,b)))

	def sequence[A](l: List[Option[A]]): Option[List[A]] = l match {
		case ho :: t => ho.flatMap(h => sequence(t).map(h :: _))
		case Nil => Some(Nil)
	}

	def traverse[A,B](l: List[A])(f: A => Option[B]): Option[List[B]] =
		l match {
			case h :: t => f(h) flatMap(ho => traverse(t)(f) map(ho :: _))
			case Nil => Some(Nil)
		}

	def sequence2[A](l: List[Option[A]]): Option[List[A]] =
		traverse(l)(identity)

}
