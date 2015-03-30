algoritmo "semnome"
// Função :
// Autor :
// Data : 31/03/2006
// Seção de Declarações 
var
Hex1, Hex2, SomaHex, MostraVaiUm : caractere
i, c, vaium, d1, d2, d3 : inteiro
inicio
// Seção de Comandos 
escreva("Digite um numero HEXA:")
leia (Hex1)
escreva("Digite outro numero HEXA:")
leia (Hex2)
// Passa tudo pra maiusculas
Hex1 <- maiusc(Hex1)
Hex2 <- maiusc(Hex2)
// Deixar os dois numeros com o mesmo comprimento - se for o caso
c <- compr(Hex1)+1
para i de c ate compr(Hex2) faca
   Hex1 <- "0" + Hex1
fimpara
c <- compr(Hex2)+1
vaium <- 0
MostraVaiUm <- " "
para i de c ate compr(Hex1) faca
   Hex2 <- "0" + Hex2
fimpara
// A soma tem que ser feita da direita para a esquerda
para i de 1 ate compr(Hex1) faca
   // C controla a posicao dos digitos sendo somados
   c <- compr(Hex1) - i + 1
   d1 <- pos(copia(Hex1,c,1),"0123456789ABCDEF") -1
   d2 <- pos(copia(Hex2,c,1),"0123456789ABCDEF") -1
   d3 <- d1 + d2 + vaium
   se d3 <= 15 entao
      MostraVaiUm <- " " + MostraVaiUm
      vaium <- 0
   senao
      MostraVaiUm <- "1" + MostraVaiUm
      vaium <- 1
      d3 <- d3 - 16
   fimse
   // Soma 1 em d3 pq 0 corresponde a 1a posicao, e assim por diante
   SomaHex <- copia("0123456789ABCDEF",d3+1,1) + SomaHex
fimpara
// Checa o ultimo vai um...
se vaium = 1 entao
   SomaHex <- "1" + SomaHex
fimse
escreval("Vai um:":10, MostraVaium:-10)
escreval("Hexa 1:":10,Hex1:-10)
escreval("Hexa 2:":10,Hex2:-10)
escreval("":10,"----------")
escreval("Soma:":10,SomaHex:-10)
fimalgoritmo
