val numbers = Vector(1, 2, 10)
val people = Vector("bla", "lala", "ok!")

numbers.head
people.tail

numbers :+ 32
"John" +: people

1 to 16 by 4

List(1, 2, 3, 4) flatMap (n => List(n))

"hola" flatMap (c => List(".", "b"))

people.max

def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)

isPrime(3)
isPrime(4)
isPrime(1)
