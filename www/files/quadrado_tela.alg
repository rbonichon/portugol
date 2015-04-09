algoritmo "Quadrado gráfico"
var n, i, j : inteiro
    linha : caractere
inicio
   repita
     escreva("Entre com um n > 0")
     leia(n)
   ate n > 0

   escreval("Primeira solução")
   para i de 1 ate n faca
     para j de 1 ate n faca
       escreva("*")
     fimpara
     escreval("")
   fimpara

   escreval("Segunda solução")
   linha <- ""
   para i de 1 ate n faca
     linha <- "*" + linha
   fimpara

   para j de 1 ate n faca
     escreval(linha)
   fimpara

   escreval("Terceira solução")
   para i de 1 ate n faca
     para j de 1 ate n - 1 faca
       escreva("*")
     fimpara
     escreval("*")
   fimpara

fimalgoritmo
