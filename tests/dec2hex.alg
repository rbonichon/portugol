algoritmo "dec2hex"
var n : inteiro

funcao convert_digit(d : inteiro) : caractere
inicio
  se d < 10 entao
    retorne carac(asc("0") + d)
  senao
    retorne carac(asc("a") + d - 10)
  fimse
fimfuncao

funcao convert(n : inteiro): caractere
var s : caractere
inicio
   se n < 16 entao
     retorne convert_digit(n)
   senao
     s <- convert(n \ 16) + convert_digit(n % 16)
     retorne s
   fimse
fimfuncao

inicio
  leia(n)
  escreval(convert(n))
fimalgoritmo
