import math.abs
/*
def product(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)
}

def factorial(x: Int): Int = product(x => x)(1, x)

//def mapReduce(n1: Int, n2: Int => Int)(unit: Int => (Int, Int) => Int) = {

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
}

mapReduce(x => x * x, (x, y) => x * y, 1)(3, 4)

product((a: Int) => a * a)(3, 4)
factorial(5)
*/
val tolerance = 0.0000001
def isCloseEnough(x: Double, y: Double) = {
  abs((x - y) / x) / x < tolerance
}
def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }

  iterate(firstGuess)
}
def averageDamp(f: Double => Double)(x: Double) = (f(x) + x) / 2

def sqrt(x: Double) = fixedPoint(averageDamp(y => x / y))(1)

sqrt(2)