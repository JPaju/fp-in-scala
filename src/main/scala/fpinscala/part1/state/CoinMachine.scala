package fpinscala.part1.state

import fpinscala.part1.state.State.{get, modify, sequence}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
	def update: Input => Machine => Machine = (i: Input) => (s: Machine) =>
		(i, s) match {
			case (_, Machine(_, 0, _)) => s
			case (Coin, Machine(false, _, _)) => s
			case (Turn, Machine(true, _, _)) => s
			case (Coin, Machine(true, candy, coin)) =>
				Machine(false, candy, coin + 1)
			case (Turn, Machine(false, candy, coin)) =>
				Machine(true, candy - 1, coin)
		}
	val processInput: Input => State[Machine, Unit] =
		modify[Machine] _ compose update
//	x => modify[Machine](update(x))

	val x: List[Input] => State[Machine, List[Unit]] =
		(l: List[Input]) => sequence(l map (modify[Machine] _ compose update))

	def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
		_ <- sequence(inputs map processInput)
		s <- get
	} yield (s.coins, s.candies)

	def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] =
		sequence(inputs map processInput)
		  .flatMap(_ => get.map(s => (s.coins, s.candies)))
}
