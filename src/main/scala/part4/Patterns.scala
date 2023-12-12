package part4

object Patterns extends App {

  trait Expr
  case class Number(n: Int) extends Expr {
    override def toString: String = {
      n.toString
    }
  }
  case class Sum(n1: Expr, n2: Expr) extends Expr {
    override def toString: String = {
      s"(${n1.toString} + ${n2.toString})"
    }
  }
  case class Prod(n1: Expr, n2: Expr) extends Expr {
    override def toString: String = {
      s"(${n1.toString} * ${n2.toString})"
    }
  }

  def translateExpr(expr: Expr): String = {
    val translate = expr match {
      case Sum(n1, n2) => s"${n1.toString} + ${n2.toString}"
      case Prod(n1, n2) => s"${n1.toString} * ${n2.toString}"
      case Number(n) => s"${n.toString}"
      case _ => ""
    }

    translate
  }

  println(translateExpr(Sum(Number(1), Number(8))))

  println(translateExpr(Prod(Number(1), Number(8))))

  println(translateExpr(Prod(Sum(Number(1), Number(8)), Number(5))))

  println(translateExpr(Prod(Sum(Number(1), Number(8)), Prod(Number(5), Number(9)))))

}
