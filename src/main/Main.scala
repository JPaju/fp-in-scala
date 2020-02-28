import part1.state.RNG.Rand
import part1.state.{RNG, SimpleRNG}


object Main {
	def main(args: Array[String]): Unit = {
		val rng = SimpleRNG(42350879)

		val x = RNG.doubleIntViaBoth
		println(x(rng))
	}
}
