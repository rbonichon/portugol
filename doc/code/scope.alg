
funcao somamatriz(n: inteiro): inteiro
var
   i, j, soma : inteiro
   foo: logico
inicio
   escreval(bar, i, j)
   soma <- 0
   para i de 1 ate 10 faca
     para j de 1 ate 10 faca
       soma <- soma + a[i,j]
     fimpara
   fimpara
   foo <- verdadeiro
   retorne soma
fimfuncao

algoritmo "semnome"
var
   i, j : inteiro
   bar: inteiro
   a : vetor [1..10,1..10] de inteiro
inicio
   bar <- 42
   para i de 1 ate 10 faca
     para j de 1 ate 10 faca
       a[i,j] <- i + j
     fimpara
   fimpara
   escreva ("Resultado: ", somamatriz(5), foo)
fimalgoritmo
