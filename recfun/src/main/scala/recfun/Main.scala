package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println("B: " + balance("(hola mundo".toList))
    println("B: " + balance("(hola mundo)".toList))
    println("B: " + balance("(hola mundo))".toList))
    println("B: " + balance("((hola mundo)".toList))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r || r < 2) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def loop(count: Int, values: List[Char]): Boolean =
      if (count < 0) false
      else if (values.isEmpty) count == 0
      else if (values.head == '(') loop(count + 1, values.tail)
      else if (values.head == ')') loop(count - 1, values.tail)
      else loop(count, values.tail)

    loop(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    var count = 0

    def loop(value: Int, r: List[Int]): Int = {
      if (r.nonEmpty) {
        if (value + r.head == money)
          count = count + 1
        else if (value + r.head < money)
          loop(value + r.head, r)

        loop(value, r.tail)
      }
      0
    }

    loop(0, coins)
    count
  }
}
