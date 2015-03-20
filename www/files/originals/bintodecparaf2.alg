// Algoritmo : BINARIO PARA DECIMAL USANDO PARA...FACA
// Função : Demonstração para o programa VisuAlg
// Autor : Claudio Morgado de Souza
// Data : 08/09/2002
algoritmo "dectobin"
// Seção de Declarações
var
x : caracter
posicao,d : inteiro
inicio
// Seção de Comandos
escreva("Entre com um número binario :")
leia(x)
d <- 0
para posicao de compr(x) ate 1 passo -1 faca
   se copia(x,posicao,1)="1" entao
      d <- d + int(exp(2,compr(x)-posicao))
   fimse
fimpara
escreval("A representação decimal de ", x, " é ", d)
fimalgoritmo

