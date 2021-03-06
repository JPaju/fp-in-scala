package fpinscala.part3

sealed trait WordCount

case class Stub(chars: String) extends WordCount

case class Part(
    lStub: String,
    words: Int,
    rStub: String
  ) extends WordCount

object WordCount {

  val monoid: Monoid[WordCount] = new Monoid[WordCount] {
    override def op(a: WordCount, b: WordCount): WordCount = (a, b) match {
      case (Stub(aa), Stub(bb))               => Stub(aa + bb)
      case (Stub(aa), Part(bl, bc, br))       => Part(aa + bl, bc, br)
      case (Part(al, ac, ar), Stub(bb))       => Part(al, ac, ar + bb)
      case (Part(al, ac, _), Part(_, bc, br)) => Part(al, ac + bc + 1, br)
    }

    override val zero = Stub("")
  }
}
