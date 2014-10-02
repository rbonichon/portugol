algoritmo "potencia iterada"
var k, p : real
    n, i : inteiro
inicio
  repita
    leia(k)
    leia(n)
   ate n >= 0

   p <- k
   i <- 0
   // i(k)
   enquanto i < n faca
     p <- p ^ k
     i <- i + 1
   fimenquanto

   escreval(n, "(", k, ") = ", p )
fimalgoritmo
