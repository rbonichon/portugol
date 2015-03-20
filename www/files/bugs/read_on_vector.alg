algoritmo "Temp"
var t: vetor [1..7] de real
    temp, m, soma : real
    dp, soma_qdif : real
    i : inteiro
inicio
   i <- 1
   soma <- 0
   repita
     escreval("Entre com um nova temperatura en C")
     leia(t[i])
     soma <- soma + t[i]
     i <- i + 1
   ate i = 8
   // media
   m <- soma / 7

   // desvio padrao
   i <- 1
   soma_qdif <- 0
   repita
     soma_qdif <- soma_qdif + (t[i] - m) * (t[i] - m)
     i <- i + 1
   ate i = 8

   dp <- raizq(soma_qdif / 7)

   escreval(m, dp)
fimalgoritmo
