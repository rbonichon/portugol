algoritmo "Potencia 3"
var k, p, s, q: real
    n, i, j, m : inteiro
inicio

   para k de 0 ate 10 faca
     para n de 0 ate 10 faca
      escreva(k, " ** ", n, " = ")
      p <- 1
      m <- n
      enquanto m > 0 faca
        j <- -1
        repita
         j <- j + 1
         q <- raizq(m - j)
        ate int(q) = q
        s <- k
        i <- m - j
        enquanto i > 1 faca
          s <- s * s
          i <- i \ 2
        fimenquanto
        p <- s * p
        m <- j
      fimenquanto
      escreval(p, " (", k^n, ")")
    fimpara
   fimpara


fimalgoritmo
