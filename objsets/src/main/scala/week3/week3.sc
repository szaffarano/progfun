abstract class IntSets {
  def incl(x: Int): IntSets

  def contains(x: Int): Boolean

  def union(other: IntSets): IntSets
}

object Empty extends IntSets {
  override def incl(x: Int): IntSets = new NonEmpty(x, Empty, Empty)

  override def union(other: IntSets): IntSets = other

  override def contains(x: Int): Boolean = false

  override def toString = "."

}

class NonEmpty(elem: Int, left: IntSets, right: IntSets) extends IntSets {

  override def incl(x: Int): IntSets =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this

  override def union(other: IntSets): IntSets =
    ((left union right) union other) incl elem

  override def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  override def toString = "{" + left + elem + right + "}"

}

def test (msg: String) = println(msg)
def test2 (msg: String) = throw new Error(msg)

val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 incl 4

test("hola")
test2("hola")