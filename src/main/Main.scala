import part1.errorhandling.{Either, Right, Left}

import scala.{Option => _, Some => _}


object Main {
	def main(args: Array[String]): Unit = {
		val eList = List(Right(1), Right(2), Right(3), Right(4), Left("Failed"))

		val x = Either.sequence(eList)
		println(x)
	}
}
