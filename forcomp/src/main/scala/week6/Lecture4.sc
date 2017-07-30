val m = Map(
  1 -> "a",
  2 -> "b"
)

val mm = m withDefaultValue "NO"

def show(k: Int): String = m get k match {
  case None => "No"
  case Some(n) => n.toString
}

class Poly(val t: Map[Int, Double]) {
  val terms = t withDefaultValue 0.0

  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  def +(other: Poly): Poly = {
    def adjust(term: (Int, Double)): (Int, Double) = {
      //      terms.get(term._1) match {
      //        case None => term
      //        case Some(n) => term._1 -> (term._2 + n)
      //      }
      val (e, c) = term
      e -> (c + terms(e))
    }

    new Poly(terms ++ (other.terms map adjust))
  }

  def altPlus(other: Poly): Poly = {
    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (e, c) = term
      terms + (e -> (c + terms(e)))
    }

    new Poly((other.terms foldLeft terms) (addTerm))
  }

  override def toString =
    (for ((exp, coef) <- terms.toList.sorted.reverse)
      yield coef + "^" + exp) mkString ("+")
}

m get 1
m get 3

show(3)
show(1)

val fruit = List("pera", "banana", "manzana", "mandarina")

val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
val p3 = new Poly(3 -> 9, 2 -> 2)

p1 + p2
p1 altPlus p2

fruit groupBy (_.head)

mm.get(444)
mm.get(1)
mm(1)
mm(444)