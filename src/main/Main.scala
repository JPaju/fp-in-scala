import scala.{Stream => _}
import part1.laziness.Stream


object Main {
	def main(args: Array[String]): Unit = {
		val s = Stream.from(1).take(5)
		val s2 = Stream.constant(34).take(3)

		val x = s.scanRight(0)(_ + _).toList
		println(x)
	}
}
