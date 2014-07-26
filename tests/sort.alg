algoritmo "C"
var v: vetor [1..10] de inteiro
    i, g, tmp: inteiro
    sh: real
    s: logico
inicio
  para i de 1 ate 10 faca
    leia(v[i])
  fimpara

  sh <- 1.3
  g <- 10
  s <- falso

  enquanto (g > 1) ou s faca
    se g > 1 entao
      g <- int(g / sh)
    fimse
    s <- falso
    para i de 1 ate (10 - g) faca
      se ((v[i] - v[i + g]) > 0) entao
        tmp <- v[i]
        v[i] <- v[i + g]
        v[i + g] <- tmp
        s <- verdadeiro
      fimse
    fimpara
  fimenquanto

  para i de 1 ate 10 faca
    escreval(v[i], " ")
  fimpara
fimalgoritmo
