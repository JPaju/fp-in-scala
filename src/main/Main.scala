import introduction.{Branch, Leaf, Tree}

object Main {
	def main(args: Array[String]): Unit = {
		val intList = introduction.List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
		val doubleList = introduction.List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)

		val intTree = Branch(
			Branch(Leaf(4), Leaf(3)), Branch(Leaf(2), Branch(Leaf(1), Leaf(5)))
		)

		val x = Tree.map(intTree)(_ + 5)
		println(x)
	}

}
