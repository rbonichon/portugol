algoritmo "Uninit array"
var
  i: inteiro
  v2, v1: vetor [1..10] de inteiro

procedimento ff(v: vetor[1..10] de inteiro)
var v : inteiro
inicio
  para i de 1 ate 10 faca
    v[i] <- i
  fimpara
fimprocedimento

procedimento ww(v: vetor[1..10] de inteiro)
inicio
  para i de 1 ate 10 faca
   escreva(v[i], " ")
  fimpara
fimprocedimento

inicio
  ff(v2)
  ww(v2)
  ff(v1)
  ww(v1)
fimalgoritmo
