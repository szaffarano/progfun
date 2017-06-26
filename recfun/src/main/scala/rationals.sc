class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator must be nonzero")

  def this(x: Int) = this(x, 1)

  private def gdc(a: Int, b: Int): Int =
    if (b == 0) a else gdc(b, a % b)

  def numer = x

  def denom = y

  def add(r: Rational) =
    new Rational(
      r.numer * denom + r.denom * numer,
      r.denom * denom
    )

  def sub(r: Rational) =
    add(r.neg)

  def mul(r: Rational) =
    new Rational(
      r.numer * numer,
      r.denom * denom
    )

  def neg: Rational = mul(new Rational(-1, 1))

  def less(r: Rational) =
    numer * r.denom < r.numer * denom

  def max(r: Rational) =
    if (this.less(r)) r else this

  override def toString() =

  val g = gdc(this.numer, this.denom)
  numer / g + "/" + denom / g
}

val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
//new Rational(1, 0)
new Rational(4)

x.add(y).mul(x)
x.sub(y).sub(z)
y.add(y)
x.less(y)
x.max(y)