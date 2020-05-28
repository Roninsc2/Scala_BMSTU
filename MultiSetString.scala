/*
Фадеев.
Лабораторная 2:
Мультимножество строк с операциями объединения ("+"),
пересечения ("*") и вычитания ("-"). В мультимножестве
одна строка может содержаться в нескольких экземплярах.
*/

class MultiSetString(d: List[String] = List()) {
  private val data = d

  def + (that: MultiSetString) = new MultiSetString(this.data ::: that.data)

  /*
  operators with list's func using
  can uses for tests

  def * (that : MultiSetString) = new MultiSetString(this.data.intersect(that.data))

  def - (that : MultiSetString) = new MultiSetString(this.data.diff(that.data))
  */

  //---no list diff func using---

  def - (that : MultiSetString) : MultiSetString = {
    if (that.data.isEmpty || this.data.isEmpty) {
      return this
    }
    val str :: otherStr = that.data
    new MultiSetString(removeStr(this.data, str)) - new MultiSetString(otherStr)
  }

  private val removeStr : (List[String], String) => List[String] = {
    case (Nil, _) => List()
    case (x :: xs, str) if (x == str) => xs
    case (x :: xs, str) => x :: removeStr(xs, str)
  }


  //---no list intersect func using---

  def * (that : MultiSetString) : MultiSetString = {
    if (that.data.isEmpty || this.data.isEmpty) {
      return new MultiSetString()
    }
    val str :: otherStr = that.data;
    val temp = new MultiSetString(intersectStr(this.data, str));
    temp + ((this - temp) * new MultiSetString(otherStr))
  }

  private val intersectStr : (List[String], String) => List[String] = {
    case (Nil, _) => List()
    case (x :: _, str) if (x == str) => List(x)
    case (_ :: xs, str) => intersectStr(xs, str)
  }

  override def toString = data.toString();
}

object MultiSetStringImplicits {
  implicit def listToMultiSetString (list: List[String]) = new MultiSetString(list);
}

object TestMultiSetString extends App {
  import  MultiSetStringImplicits._
  var c  = List("5");
  var a = new MultiSetString(List("1"));
  var b = c + a + c + a
  println("b = ", b); //List(5, 1, 5, 1)
  println("a = ", a); //List(1)
  println("b-a = ", b - a); // List(5, 5, 1)
  println("a-b = ", a - b); //List()
  println("b*a = ", b * a); //List(1)
  println("a*b = ", a * b); //List(1)
  a += a // a = List(1, 1)
  println("b-a = ", b - a); // List(5, 5)
  println("a-b = ", a - b); // List()
  println("b*a = ", b * a); // List(1, 1)
  println("a*b = ", a * b); // List(1, 1)
}


