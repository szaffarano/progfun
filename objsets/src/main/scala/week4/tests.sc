import week4.List
import week4.Cons
import week4.Nil

def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

def nth[T](idx: Int, list: List[T]): T = {
  if (list.isEmpty) throw new IndexOutOfBoundsException
  if (idx == 0) list.head
  else nth(idx - 1, list.tail)
}

val l = singleton(1)

nth(2, l)
nth(-1, l)