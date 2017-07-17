/**
  * Created by sebas on 7/15/17.
  */
trait Expr {
  def eval: Int = {
    this match {
      case Number(n) => n
      case Sum(e1, e2) => e1.eval + e2.eval
      case Prod(e1, e2) => e1.eval * e2.eval
      case Var(x) => throw new Error("Var: " + x)
    }
  }

  def show: String = this match {
    case Number(n) => n.toString
    case Sum(e1, e2) => e1.show + " + " + e2.show
    case Prod(e1, e2) =>
      (e1 match {
        case Sum(_, _) => "(" + e1.show + ")"
        case _ => e1.show
      }) + " * " + (e2 match {
        case Sum(_, _) => "(" + e2.show + ")"
        case _ => e2.show
      })
    case Var(s) => s
  }
}

case class Number(n: Int) extends Expr

case class Sum(left: Expr, right: Expr) extends Expr

case class Var(s: String) extends Expr

case class Prod(e1: Expr, e2: Expr) extends Expr

object Test {
  def main(args: Array[String]): Unit = {
    println(Prod(Sum(Number(1), Number(1)), Number(2)).eval)
    println(Prod(Sum(Number(1), Number(1)), Number(2)).show)
    println(Sum(Prod(Number(2), Var("x")), Var("y")).show)
    println(Prod(Sum(Number(2), Var("x")), Var("y")).show)
    println(Prod(Sum(Number(1), Number(2)), Number(3)).show)
  }
}
