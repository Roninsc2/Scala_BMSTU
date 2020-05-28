/*
Фадеев.
Лабораторная 1:
Функция coprimes: List[Int] => List[(Int, Int)],
выполняющая поиск в списке целых чисел пар взаимно
простых чисел. Функция должна возвращать список
найденных пар, причём в каждой паре первое число
должно быть меньше второго.
*/

object Coprimes {

  def coprimes(list: List[Int]): List[(Int, Int)] = {
    if (list == Nil) {
      List()
    } else {
      val l :: ls = list
      coprime(l, ls) ::: coprimes(ls)
    }
  }

  private def gcd(x: Int, y: Int): Int = {
    if (y == 0) x else gcd(y, x % y)
  }

  private val coprime: (Int, List[Int]) => List[(Int, Int)] = {
    case (_, Nil) => List()
    case (x, l :: ls) if (gcd(x, l) == 1) => {
      if (x < l) {
        (x, l) :: coprime(x, ls)
      } else {
        (l, x) :: coprime(x, ls)
      }
    }
    case (x, _ :: ls) => coprime(x, ls)
  }

}

object TestCoprimes extends App {
  import Coprimes._
  val a = List(3, 1, 9, 7, 6)
  println(coprimes(a))
  /*
  1 coprime with 3, 6, 7, 9
  3 coprime with 1, 7
  6 coprime with 1, 7
  9 coprime with 1, 7
  7 coprime with 1, 3, 6, 9

  Output: List((1,3), (3,7), (1,9), (1,7), (1,6), (7,9), (6,7))
  */
}