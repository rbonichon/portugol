algoritmo "S"
var
   u, i, n, tv, tva, amax : inteiro
inicio
   repita
      escreva("Give a value > 0: ")
      leia(n)
   ate n > 0
   u <- n
   i <- 0
   tv <- 0
   tva <- 0
   amax <- 0
   enquanto u <> 1 faca
     i <- i + 1
     se ((u % 2) = 1) entao
       u <- 3 * u + 1
     senao
       u <- u \ 2
     fimse
     escreval("u", i, " = ", u)
     se (u = 1) e (tv = 0) entao
         tv <- i
     fimse
     se (u <= n) e (tva = 0) entao
        tva <- i
     fimse
     se (tv = 0) e (u > amax) entao
        amax <- u
     fimse
  fimenquanto
  escreva("tempo de voo:", tv, "em altitude", tva, "alt max:", amax)
fimalgoritmo
