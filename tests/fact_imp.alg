algoritmo "fact"
var n, res: inteiro
inicio
  repita
   leia(n)
  ate n >= 0
  res <- 1
  enquanto n > 1 faca
    res <- n * res
    n <- n - 1
  fimenquanto
  escreva(res)
fimalgoritmo
