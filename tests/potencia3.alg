algoritmo "Potencia 3"
var k, p, s, q: real
    n, i, j, m : inteiro
inicio

   para k de 0 ate 10 faca
     para n de 0 ate 10 faca
      escreva(k, " ** ", n, " = ")
      i <- n
      p <- 1
      s <- k
      enquanto i > 0 faca
       se i % 2 = 1 entao
         p <- p * s
         i <- i - 1
       fimse
        s <- s * s
        i <- i \ 2
      fimenquanto
      escreval(p, " (", k^n, ")")
    fimpara
   fimpara


fimalgoritmo
