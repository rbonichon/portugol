algoritmo "Test expressions"
incluia "libtest.alg"

inicio
  test_init()
  ltest_eq(10 % 5 * 2 <>  5 * 2 + 1, verdadeiro)
  ltest_eq(52 % 7 - 47 > -2 * 4 + - 35.3, falso)
  ltest_eq(12 + 5 * 2 - 8 > 8 + 6 <> verdadeiro, verdadeiro)
  itest_eq(5 * 3 + 1 - 4 * 3 / 2, 10)
  itest_eq(7 % 3 \ 2 * 7.5 + 1 / 3, 1 / 3)
  itest_eq(52 \ 7 - 47 \ - 2 * 4 + - 3, 96)
  escreval(-3. + 5 * 2 % 5 / 7 - - 3.2)
  escreval(7 % 3 \ 2 * 7.5 + 1 / 3)
  escreval(57 \ 7 - 19 % - 3 * 4 - 3)
  test_end()
  // escreva(int(5 / 2) + 1 = falso)
fimalgoritmo
