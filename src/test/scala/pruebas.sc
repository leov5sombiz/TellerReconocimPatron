  import Newton._

  val expr1 = Suma(Atomo('x'), Numero(2))
  val expr2 = Prod(Atomo('x'), Atomo('x'))
  val expr3 = Logaritmo(Atomo('x'))
  val expr4 = Expo(Atomo('x'), Numero(3))
  val expr5 = Suma(expr1, Expo(expr2, Numero(5)))
  val expr6 = Div(expr2, Atomo('x'))
  val expr7 = Prod(Atomo('y'), Atomo('y'))
  val expr8 = Resta(Numero(10), expr2)


  mostrar(expr1)
  mostrar(expr2)
  mostrar(expr3)
  mostrar(expr4)
  mostrar(expr5)
  mostrar(expr6)
  mostrar(expr7)
  mostrar(expr8)


  mostrar(derivar(expr1,Atomo('x')))
  mostrar(derivar(expr2,Atomo('x')))
  mostrar(derivar(expr3,Atomo('x')))
  mostrar(derivar(expr4,Atomo('x')))
  mostrar(derivar(expr6,Atomo('x')))
  mostrar(derivar(expr7,Atomo('y')))


  mostrar(Numero(5.0))
  evaluar(Numero(5.0), Atomo('x'), 1.0)

  mostrar(Atomo('x'))
  evaluar(Atomo('x'), Atomo('x'), 5.0)

  mostrar(Suma(expr1, expr2))
  evaluar(Suma(expr1, expr2), Atomo('x'), 5.0)

  mostrar(Prod(expr1, expr2))
  evaluar(Prod(expr1, expr2), Atomo('x'), 5.0)

  mostrar(Resta(expr1, expr2))
  evaluar(Resta(expr1, expr2), Atomo('x'), 5.0)


  limpiar(derivar(expr1,Atomo('x')))
  mostrar(limpiar(derivar(expr1,Atomo('x'))))

  limpiar(derivar(expr2,Atomo('x')))
  mostrar(limpiar(derivar(expr2,Atomo('x'))))

  limpiar(derivar(expr4,Atomo('x')))
  mostrar(limpiar(derivar(expr4,Atomo('x'))))

  limpiar(derivar(expr6,Atomo('x')))
  mostrar(limpiar(derivar(expr6,Atomo('x'))))

  limpiar(derivar(expr7,Atomo('y')))
  mostrar(limpiar(derivar(expr7,Atomo('y'))))


  def buenaAprox(f: Expr, a: Atomo, d: Double): Boolean = {
    Math.abs(evaluar(f, a, d)) < 0.0001}

  raizNewton(expr1, Atomo('x'), 2.0, buenaAprox)
  raizNewton(expr2, Atomo('x'), 2.0, buenaAprox)
  raizNewton(expr3, Atomo('x'), 2.0, buenaAprox)
  raizNewton(expr4, Atomo('x'), 2.0, buenaAprox)
  raizNewton(expr8, Atomo('x'), 3.0, buenaAprox)