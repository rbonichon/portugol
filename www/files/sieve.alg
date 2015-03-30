algoritmo "E"
var v: vetor [2 .. 50] de logico
    i, j, lidx: inteiro
inicio
   lidx <- 50

   para i de 2 ate lidx faca
     v[i] <- verdadeiro
   fimpara

   para i de 2 ate lidx faca
     se v[i] entao
        j <- i + i
        enquanto j <= lidx faca
          v[j] <- falso
          j <- j + i
        fimenquanto
     fimse
   fimpara

   para i de 2 ate lidx faca
     se v[i] entao
        escreva(i, " ")
     fimse
   fimpara
fimalgoritmo
