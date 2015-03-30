algoritmo "Logic"
var p, q, res : logico
    i : inteiro
inicio
    i <- 0
    enquanto i < 4 faca
      p <- (i \ 2) = 1
      q <- i % 2 = 1
      res <- p ou nao q
      escreval(p, " ou ", q, " = ", res)
      i <- i + 1
    fimenquanto
fimalgoritmo
