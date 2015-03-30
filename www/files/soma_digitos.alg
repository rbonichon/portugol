algoritmo "Soma de algarimos"
var d1, d2, n: inteiro
inicio
  leia(n) // n = d1 d2 d3 d4
  //d4 <- n % 10
  n <- n \ 10
  //d3 <- n % 10
  n <- n \ 10
  d2 <- n % 10
  d1 <- n \ 10
  se (d1 + d2) % 2 = 0
  entao
     escreva ("sim")
  senao
     escreva ("nao")
  fimse
fimalgoritmo
