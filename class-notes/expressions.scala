package week4

trait Expr {

  def show(e:Expr):String = e match {
    case Number(n) => n.toString
    case Sum(l,r) => show(l) + show(r)
  }

  def eval(e:Expr): Int = {
    this match {
      case Number(n) => n
      case Sum(e1, e2) => e1.eval + e2.eval
    }
  }
}

case class Number(n: Int) extends Expr {
}
case class Sum(e1:Expr, e2:Expr) extends Expr {

}



