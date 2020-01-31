package introduction


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
		case Some(v) if (p(v)) => this
		case None => None
	}

}

case class Some[+A](v: A) extends Option[A]
case object None extends Option[Nothing]

