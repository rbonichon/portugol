// Algoritmo : DECIMAL PARA BINARIO USANDO PARA...FACA
// Função : Demonstração para o programa VisuAlg
// Autor : Claudio Morgado de Souza
// Data : 08/09/2002
algoritmo "dectobin"
// Seção de Declarações
var
x : caracter
j,y,d : inteiro
inicio
// Seção de Comandos
escreva("Entre com um número inteiro :")
leia(y)
d <- y
para j de 1 ate 10 faca
   se (y mod 2) = 0 entao
      x <-  "0" +  x
   senao
      x <-  "1" +  x
   fimse
   y <- y div 2
fimpara
escreval("A representação binária de", d, " é ", x)
fimalgoritmo

