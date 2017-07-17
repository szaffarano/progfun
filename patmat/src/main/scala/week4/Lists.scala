/**
  * Created by sebas on 7/16/17.
  */
class Lists {
}

object TestList {
  def isort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, isort(ys))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x > y) y :: insert(x, ys) else x :: xs
  }

  def main(args: Array[String]): Unit = {
    isort(List(99, 1, 2, 5, 3)).foreach(x => println(x))
    isort(List(1, 2, 3, 5, 99)).foreach(x => println(x))
    val c = List('a', 'b')
    val nn: List[Char] = List('d') ::: c
    println(c)
    println(nn)
  }
}