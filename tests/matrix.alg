algoritmo "matrix"
var m : vetor [1..10, 1..10] de inteiro
    i, j:inteiro
inicio

para i de 1 ate 10 faca
  para j de 1 ate 10 faca
    m[i, j] <- i * j
  fimpara
fimpara


para i de 1 ate 10 faca
  para j de 1 ate 10 faca
    escreval(m[i, j], " ")
  fimpara
fimpara


fimalgoritmo
