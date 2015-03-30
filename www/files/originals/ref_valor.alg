algoritmo "semnome"
// Função :
// Autor :
// Data : 5/8/2014
// Seção de Declarações 
var
   m, x, y: inteiro
   
funcao h(a, b:inteiro):inteiro
inicio
      m <- a + b
      retorne m
fimfuncao

funcao g(var m:inteiro; n:inteiro): inteiro
inicio
      m <- n
      n <- n + n
      retorne m * n
fimfuncao

funcao f(o:inteiro; var n: inteiro): inteiro
var p: inteiro
inicio
    p <- g(o, n)
    m <- h(o, n)
    n <- h(p, m)
    retorne m + n
fimfuncao

inicio
   x <- 1
   y <- 2
   escreva(f(x, y))
   escreva(m, x, y)
fimalgoritmo
