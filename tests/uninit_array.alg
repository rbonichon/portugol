algoritmo "Uninit array"
var
  i, j: inteiro
  v, v1: vetor [1..10] de inteiro

procedimento f(v: vetor[1..10] de inteiro)
inicio
  para i de 1 ate 10 faca
    v[i] <- i
  fimpara
fimprocedimento

procedimento w(v: vetor[1..10] de inteiro)
inicio
  para i de 1 ate 10 faca
   escreva(v[i], " ")
  fimpara
fimprocedimento

inicio
  f(v)
  w(v)
  f(v1)
  w(v1)
fimalgoritmo
