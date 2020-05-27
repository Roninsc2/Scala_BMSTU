/*
* Для булевского типа умножение – это конъюнкция,
сложение – дизъюнкция, и порядок задан как false < true.
* */

abstract class Coeff[T] {
  def *(a: T, b: T): T

  def +(a: T, b: T): T

  def <(a: T, b: T): Boolean
}

object Coeff {

  implicit object int extends Coeff[Int] {
    override def +(a: Int, b: Int): Int = a + b

    override def *(a: Int, b: Int): Int = a * b

    override def <(a: Int, b: Int): Boolean = a < b
  }

  implicit object bool extends Coeff[Boolean] {
    override def *(a: Boolean, b: Boolean): Boolean = a && b

    override def +(a: Boolean, b: Boolean): Boolean = a || b

    override def <(a: Boolean, b: Boolean): Boolean = a < b
  }

}

class LineEq[T](a: List[T], x: List[T], b: T) {
  private val alist = a
  private val xlist = x
  private val bval = b
  def calc() (implicit calc : Coeff[T]) : Boolean = {
    calc.<(left(alist, xlist), bval)
  }

  private def left(alist: List[T], xlist: List[T]) (implicit calc : Coeff[T]) : T = {
    val a :: as = alist
    val x :: xs = xlist
    if (alist.size == 1) {
      calc.*(a, x)
    } else {
      calc.+(calc.*(a, x), left(as, xs))
    }
  }
}

object TestCoeff extends App {
  val a = List(1, 1, 1)
  val x = List(3, 3, 3)
  val b = 4
  val l = new LineEq(a, x, 10);
  println(l.calc())
}

