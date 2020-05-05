import java.util.concurrent.{ExecutorService, Executors}

import part2.parallelism.Par

import scala.util.Random

object Main {

	implicit val es: ExecutorService = Executors.newCachedThreadPool()

	def main(args: Array[String]): Unit = {
		val rnd = List.fill(10)(Random.nextInt % 20)
		val par = Par.foldRight(rnd, Int.MinValue)((curr, max) => if (curr > max) curr else max)

		println(s"Main thread is: ${Thread.currentThread()}")

		val res = Par.run(par)
		println(s"Result is: $rnd")

		es.shutdown()
	}
}
