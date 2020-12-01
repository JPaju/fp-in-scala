package fpinscala.part1.gettingstarted

object GettingStarted {

	def main(args: Array[String]): Unit = {
		val curried = curry(add)
		val add2 = curried(2)
		val add10 = curried(10)
		val add12 = add2 compose add10

		println(factorial(8))
	}

	def fib(n: Int): Int = {
		@annotation.tailrec
		def loop(n: Int, prev: Int, curr: Int): Int =
			if (n == 0) prev
			else loop(n - 1, curr, prev + curr)

		loop(n, 0, 1)
	}

	def factorial(n: Int): Long = {
		@annotation.tailrec
		def loop(n: Int, acc: Int): Int =
			if (n == 0) acc
			else loop(n - 1, acc * n)
		loop(n, 1)
	}

	def curry[A, B, C](f: (A, B) => C): A => (B => C) =
		a => b => f(a, b)

	def uncurry[A, B, C](f: A => B => C): (A, B) => C =
		(a, b) => f(a)(b)

	def compose[A, B, C](f: B => C, g: A => B): A => C =
		a => f(g(a))

	def add(a: Int, b: Int): Int = a + b
}


