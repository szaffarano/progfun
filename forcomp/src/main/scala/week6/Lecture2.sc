val n = 7

def isPrime(n: Int): Boolean = (2 until n) forall (n % _ != 0)

((1 until n) map (i =>
  (1 until i) map (j => (i, j)))).flatten

(1 until n) flatMap (i =>
  (1 until i) map (j => (i, j))) filter (p => isPrime(p._1 + p._2))

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

def scalarProduct(xs: List[Int], ys: List[Int]): Int =
  (for ((x, y) <- xs zip ys ) yield (x * y)).sum