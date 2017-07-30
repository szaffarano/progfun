def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y :: ys => last(ys)
}

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case x :: xs => x :: init(xs)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case z :: zs => reverse(zs) ::: List(z)
}

def removeAt[T](n: Int, xs: List[T]): List[T] =
  (xs take n) ::: (xs drop n + 1)

val l1 = List(1, 2, 3)
val l2 = List(4, 5, 6)



assert(List(1).init == init(List(1)))


assert(last(l1) == l1.last)
assert(l1.init == init(l1))
assert(l1 ::: l2 == concat(l1, l2))
assert(l1.reverse == reverse(l1))
assert(List().reverse == reverse(List()))