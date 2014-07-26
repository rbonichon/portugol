// Algoritmo : BINARIO PARA DECIMAL USANDO PARA...FACA
// Função : Demonstração para o programa VisuAlg
// Autor : Claudio Morgado de Souza
// Data : 08/09/2002
algoritmo "dectobin"
// Seção de Declarações
var
x : caracter
j,posicao,expoente,d : inteiro
inicio
// Seção de Comandos
escreva("Entre com um número binario :")
leia(x)
d <- 0
expoente <- 0
para j de 1 ate compr(x) faca
   posicao <- compr(x) - j + 1
   se copia(x,posicao,1)="1" entao
      d <- d + int(exp(2,expoente))
   fimse
   expoente <- expoente + 1
fimpara
escreval("A representação decimal de ", x, " é ", d)
fimalgoritmo

