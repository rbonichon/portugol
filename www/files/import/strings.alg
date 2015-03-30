funcao reverse(txt: caractere): caractere
var i, len : inteiro
    t, s : caractere
inicio
   len <- compr(txt)
   s <- " "
   para i de 1 ate len faca
    t <- copia(txt, i, 1)
    s <- t + s
   fimpara
   retorne s
fimfuncao
