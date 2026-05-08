import Newton._

val expr1 = Numero(2)
val expr2 = Numero(4)
val expr3 = Suma(expr1,expr2)
val expr4 = Resta(expr1,expr2)
val expr5 = Prod(expr1, expr2)

mostrar(expr1)
mostrar(expr2)
mostrar(expr3)
mostrar(expr4)
mostrar(expr5)