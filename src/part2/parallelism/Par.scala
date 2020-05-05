package part2.parallelism

import java.util.concurrent.{ExecutorService, Future, TimeUnit}

object Par {

	type Par[A] = ExecutorService => Future[A]

	private case class UnitFuture[A](get: A) extends Future[A] {
		def isDone = true
		def get(timeout: Long, units: TimeUnit): A = get
		def isCancelled = false
		def cancel(evenIfRunning: Boolean): Boolean = false
	}

	def unit[A](a: A): Par[A] = _ => UnitFuture(a)
	def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
	def fork[A](a: => Par[A]): Par[A] = es => es.submit(() => a(es).get)

	def run[A](p: Par[A])(implicit es: ExecutorService): Future[A] = p(es)

	def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A,B) => C): Par[C] =
		es => {
			val af = pa(es)
			val bf = pb(es)
			UnitFuture(f(af.get, bf.get))
		}

	def asyncF[A,B](f: A => B): A => Par[B] =  a => lazyUnit(f(a))

	def map[A,B](pa: Par[A])(f: A => B): Par[B] =
		map2(pa, unit(()))((a, _) => f(a))

	def sortPar(pl: Par[List[Int]]): Par[List[Int]] = map(pl)(_.sorted)

	def sequence[A](p: List[Par[A]]): Par[List[A]] =
		p.foldRight(unit(Nil): Par[List[A]])((pa, b) => map2(pa, b)(_ :: _))

	def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
		val fbs = ps.map(asyncF(f))
		sequence(fbs)
	}

	def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = lazyUnit {
		as.filter(f)
	}

	def foldRight[A,B](p: List[A], z:B)(f: (A, B) => B): Par[B] = lazyUnit {
		p.foldRight(z)(f)
	}

	def equal[A](p: Par[A], p2: Par[A])(implicit es: ExecutorService): Boolean =
		p(es).get == p2(es).get

	def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

	def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
		es =>
			if (run(cond)(es).get) t(es)
			else f(es)

	def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
		es => choices(run(n)(es).get)(es)

	def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
		choiceN(map(cond)(c => if(c) 0 else 1))(List(t,f))

	//	flatMap
	def chooser[A,B](p: Par[A])(choices: A => Par[B]): Par[B] =
		es => choices(run(p)(es).get)(es)

	def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
		chooser(cond)(if (_) t else f)

	def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
		chooser(n)(choices(_))

	def flatMap[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
		chooser(p)(f)

	def join[A](a: Par[Par[A]]): Par[A] =
		es => run(a)(es).get()(es)

	def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
		join(map(p)(f))

	def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
		flatMap(a)(p => p)

}
