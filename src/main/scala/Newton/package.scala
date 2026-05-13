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

  /**
   * Ejercicio #1: Función Mostrar
   * @param e: Expresión ya sea númerica o variable
   * @return Convierte el parámetro en String para mostrarlo en la consola
   */
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

  /**
   * Ejercicio #2: Función Derivar
   * @param f: Expresión a derivar
   * @param a: Variable respecto a la cual se deriva la expresión
   * @return Expresión correspondiente a la derivada aplicando reglas de derivación
   */
  def derivar(f: Expr, a: Atomo): Expr = f match {

    case Numero(_) =>
      Numero(0)

    case Atomo(x) =>
      if (x == a.x) Numero(1)
      else Numero(0)

    case Suma(e1, e2) =>
      Suma(derivar(e1, a), derivar(e2, a))

    case Resta(e1, e2) =>
      Resta(derivar(e1, a), derivar(e2, a))

    case Prod(e1, e2) =>
      Suma(
        Prod(derivar(e1, a), e2),
        Prod(e1, derivar(e2, a))
      )

    case Div(e1, e2) =>
      Div(
        Resta(
          Prod(derivar(e1, a), e2),
          Prod(e1, derivar(e2, a))
        ),
        Expo(e2, Numero(2))
      )

    case Logaritmo(e) =>
      Div(derivar(e, a), e)

    case Expo(e1, e2) =>
      Prod(
        Expo(e1, e2),
        Suma(
          Div(
            Prod(derivar(e1, a), e2),
            e1
          ),
          Prod(
            derivar(e2, a),
            Logaritmo(e1)
          )
        )
      )
  }

  /**
   * Ejercicio #3: Función Evaluar
   * @param f: Expresión matemática a evaluar
   * @param a: Variable
   * @param v: Valor con el que se evalúa la expresión
   * @return Imagen de la función evaluada en "v"
   */
  def evaluar(f: Expr, a: Atomo, v: Double): Double = f match {

    case Numero(d) =>
      d

    case Atomo(x) =>
      v

    case Suma(e1, e2) =>
      evaluar(e1, a, v) +
        evaluar(e2, a, v)

    case Prod(e1, e2) =>
      evaluar(e1, a, v) *
        evaluar(e2, a, v)

    case Resta(e1, e2) =>
      evaluar(e1, a, v) -
        evaluar(e2, a, v)

    case Div(e1, e2) =>
      evaluar(e1, a, v) /
        evaluar(e2, a, v)

    case Expo(e1, e2) =>
      Math.pow(
        evaluar(e1, a, v),
        evaluar(e2, a, v)
      )

    case Logaritmo(e) =>
      Math.log(
        evaluar(e, a, v)
      )
  }

  /**
   * Ejercicio #4: Función Limpiar
   * @param f: Expresión matemática a limpiar
   * @return Expresión simplificada de vforma que se vea más prolija
   */
  def limpiar(f: Expr): Expr = f match {

    case Numero(d) =>
      Numero(d)

    case Atomo(x) =>
      Atomo(x)

    case Suma(e1, e2) =>

      (limpiar(e1), limpiar(e2)) match {

        case (Numero(0), b) =>
          b

        case (a, Numero(0)) =>
          a

        case (a, b) =>
          Suma(a, b)
      }

    case Resta(e1, e2) =>

      (limpiar(e1), limpiar(e2)) match {

        case (a, Numero(0)) =>
          a

        case (a, b) =>
          Resta(a, b)
      }

    case Prod(e1, e2) =>

      (limpiar(e1), limpiar(e2)) match {

        case (Numero(0), _) =>
          Numero(0)

        case (_, Numero(0)) =>
          Numero(0)

        case (Numero(1), b) =>
          b

        case (a, Numero(1)) =>
          a

        case (a, b) =>
          Prod(a, b)
      }

    case Div(e1, e2) =>

      (limpiar(e1), limpiar(e2)) match {

        case (Numero(0), _) =>
          Numero(0)

        case (a, Numero(1)) =>
          a

        case (a, b) =>
          Div(a, b)
      }

    case Expo(e1, e2) =>

      (limpiar(e1), limpiar(e2)) match {

        case (_, Numero(0)) =>
          Numero(1)

        case (a, Numero(1)) =>
          a

        case (a, b) =>
          Expo(a, b)
      }

    case Logaritmo(e) =>
      Logaritmo(limpiar(e))
  }

  /**
   * Ejercicio #5: Función raizNewton
   * @param f: Expresión a sacar raíces
   * @param a: Variable a tener en cuenta
   * @param x0: Aproximación inicial
   * @param ba: Indica si ya se está lo suficientemente cerca de la raíz
   * @return
   */

  def raizNewton(
                  f: Expr,
                  a: Atomo,
                  x0: Double,
                  ba: (Expr, Atomo, Double) => Boolean
                ): Double = {

    if (ba(f, a, x0))
      x0

    else {

      val df = derivar(f, a)

      val x1 =
        x0 - (
          evaluar(f, a, x0) /
            evaluar(df, a, x0)
          )

      raizNewton(f, a, x1, ba)
    }
  }

}
