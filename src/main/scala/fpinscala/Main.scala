package fpinscala

import fpinscala.part1.state.RNG
import fpinscala.part2.testing.{ Gen, Prop }
import fpinscala.part2.testing.Gen.stringN
import fpinscala.part3.monoids.{ Monoid, Part, Stub, WordCount }

object Main {

  def main(args: Array[String]): Unit = {
    val stringGen   = stringN(5)
    val oneToTenGen = Gen.choose(1, 10)

    val partGen = oneToTenGen.map2(for {
      s1 <- stringGen
      s2 <- stringGen
    } yield (s1, s2)) { case (c, (s1, s2)) => Part(s1, c, s2) }
    val stubGen = stringGen.map(Stub)
    val wcGen   = Gen.union(partGen, stubGen)

    val wcMonoidProps = Monoid.monoidLaws(WordCount.monoid, wcGen)
    Prop.run(wcMonoidProps, rng = RNG.SimpleRNG(2))
  }
}
