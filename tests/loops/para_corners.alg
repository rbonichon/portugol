algoritmo "Teste laco para"
var
    i, j : inteiro
    t : caractere

procedimento next()
inicio
  escreval("")
  j <- j + 1
  escreval(j, ":")
fimprocedimento

inicio

  next()
  para i de 10 ate 1 passo 0 faca
    escreva(i)
  fimpara

  next()
  para i de 1 ate 10 faca
    escreva(i)
  fimpara
  escreva(i)

  next()
  para i de 1 ate 10 passo 2 faca
    escreva(i)
  fimpara

  next()
  para i de 1 ate 10 faca
    escreva(i)
    i <- i * 2
  fimpara

  next()
  para i de 1 ate  10 passo -2 faca
    escreva(i)
  fimpara

  next()
  para i de 100 ate  10 passo 2 faca
    escreva(i)
  fimpara

fimalgoritmo
