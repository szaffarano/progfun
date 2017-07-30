import math.Ordering

def msortOld[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }
    val (l, r) = xs.splitAt(n)
    merge(msortOld(l)(lt), msortOld(r)(lt))
  }
}



def msortOld2[T](xs: List[T])(ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (ord.lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }
    val (l, r) = xs.splitAt(n)
    merge(msortOld2(l)(ord), msortOld2(r)(ord))
  }
}

def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (ord.lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }
    val (l, r) = xs.splitAt(n)
    merge(msort(l)(ord), msort(r)(ord))
  }
}

val l1 = List(4, 1, 99, 44, 0, 2)
val l2 = List("a", "z", "n", "b")

def lt1 = (a: Int, b: Int) => (a < b)
def lt2 = (a: String, b: String) => (a.compareTo(b) < 0)

assert(msortOld(l1)(lt1) == l1.sortWith(lt1))
assert(msortOld(l2)(lt2) == l2.sortWith(lt2))

assert(msortOld2(l1)(Ordering.Int) == l1.sortWith(lt1))
assert(msortOld2(l2)(Ordering.String) == l2.sortWith(lt2))

assert(msort(l1) == l1.sortWith(lt1))
assert(msort(l2) == l2.sortWith(lt2))
