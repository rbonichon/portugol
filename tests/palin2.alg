algoritmo "semnome"
var
   max, n1, n2, n, p, invn, dgt: inteiro
   x, palin: logico
inicio
      max <- 0
      para n1 de 100 ate 999 faca
           para n2 de n1 ate 999 faca
                n <- n1 * n2
                p <- n
                invn <- 0
                enquanto p <> 0 faca
                         dgt <- p % 10
                         p <- p \ 10
                         invn <- (10 * invn) + dgt
                fimenquanto
                palin <- n = invn
                se palin entao
                   se n > max entao
                      max <- n
                   fimse
                fimse
           fimpara
      fimpara
      escreval(max)
fimalgoritmo
