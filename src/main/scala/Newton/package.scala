package object Newton {

  trait Expr
  case class Numero(d: Double) extends Expr
  case class Atomo(x: Char) extends Expr
  case class Suma(e1: Expr, e2: Expr) extends Expr
  case class Resta(e1: Expr, e2: Expr) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr
  case class Div(e1: Expr, e2: Expr) extends Expr
  case class Expo(e1: Expr, e2: Expr) extends Expr
  case class Logaritmo(e1: Expr) extends Expr

  def mostrar(e: Expr): String = e match {

    case Numero(d) =>
      d.toString

    case Atomo(x) =>
      x.toString

    case Suma(a, b) =>
      "(" + mostrar(a) + " + " + mostrar(b) + ")"

    case Prod(a, b) =>
      "(" + mostrar(a) + " * " + mostrar(b) + ")"

    case Resta(a, b) =>
      "(" + mostrar(a) + " - " + mostrar(b) + ")"

    case Div(a, b) =>
      "(" + mostrar(a) + " / " + mostrar(b) + ")"

    case Expo(a, b) =>
      "(" + mostrar(a) + " ^ " + mostrar(b) + ")"

    case Logaritmo(a) =>
      "(lg(" + mostrar(a) + "))"
  }
}
