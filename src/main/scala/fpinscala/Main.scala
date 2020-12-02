package fpinscala

import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.part1.state.{RNG, State}
import fpinscala.part2.parallelism.Par
import fpinscala.part2.testing.{Gen, Prop}
import fpinscala.part3.Monoid

import scala.util.Random

object Main {

	def main(args: Array[String]): Unit = {
		val booleanGen = Gen(State(RNG.boolean))
		val intGen = Gen(State(RNG.int))
		val intListGen = intGen.listOfN(5)

		val intAdditionMonoidProps = Monoid.monoidLaws(Monoid.intAddition, intGen)
		val intMultiplicationMonoidProps = Monoid.monoidLaws(Monoid.intMultiplication, intGen)
		val intListMonoidProps = Monoid.monoidLaws(Monoid.listMonoid[Int], intListGen)
		val booleanAndMonoidProps = Monoid.monoidLaws(Monoid.booleanAnd, booleanGen)
		val booleanOrMonoidProps = Monoid.monoidLaws(Monoid.booleanOr, booleanGen)

		Prop.run(intAdditionMonoidProps)
		Prop.run(intMultiplicationMonoidProps)
		Prop.run(intListMonoidProps)
		Prop.run(booleanAndMonoidProps)
		Prop.run(booleanOrMonoidProps)
	}
}
