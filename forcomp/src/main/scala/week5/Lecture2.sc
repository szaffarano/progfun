def msort(xs: List[Int]): List[Int] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    //    def merge1(xs: List[Int], ys: List[Int]): List[Int] = xs match {
    //      case Nil => ys
    //      case x :: xs1 => {
    //        ys match {
    //          case Nil => xs
    //          case y :: ys1 =>
    //            if (x < y) x :: merge1(xs1, ys)
    //            else y :: merge1(xs, ys1)
    //        }
    //      }

    def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (x < y) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }
  }

  def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Nil, _) => ys
    case (_, Nil) => xs
    case (x :: xs1, y :: ys1) =>
      if (x < y) x :: merge(xs1, ys)
      else y :: merge(xs, ys1)
  }

  val (f, s) = xs.splitAt(n)
  merge(msort(f), msort(s))
}

val l1 = List(3, 2, 1, 1, 45, 2)
val pair = ("HI", 1)
val (l, v) = pair

assert(msort(l1) == l1.sortWith((a, b) => a < b))