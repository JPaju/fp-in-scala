import part1.errorhandling.{Option, Some, None}

import scala.{Option => _, Some => _}


object Main {
	def main(args: Array[String]): Unit = {
		val oList = List(Some(1), Some(2), Some(3), Some(4))

		val x = Option.sequence2(oList)
		println(x)
	}
}
