algoritmo "ack"
var m, n : inteiro

funcao ack (m, n: inteiro): inteiro
inicio
   escreval("    ", m, " ", n)
   se m = 0 entao retorne n + 1
   senao se n = 0 entao retorne ack (m - 1, 1)
   senao retorne ack (m - 1, ack (m, n - 1))
   fimse fimse
fimfuncao

inicio
  leia(m,n)
  escreva (ack(m, n))
fimalgoritmo
