algoritmo "Test arithmetic"
incluia "libtest.alg"
inicio
  test_init()
  ltest_eq(10 % 5 * 2 <>  5 * 2 + 1, verdadeiro)
  ltest_eq(52 % 7 - 47 > -2 * 4 + - 35.3, falso)
  ltest_eq(12 + 5 * 2 - 8 > 8 + 6 <> verdadeiro, verdadeiro)
  test_end()
  // escreva(int(5 / 2) + 1 = falso)
fimalgoritmo
