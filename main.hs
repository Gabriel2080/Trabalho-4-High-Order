-- 1. Escreva  uma  função  chamada  fatorialn  que  usando  o  operador  range  e  a  função  foldr devolva o fatorial de n.

fatorialn :: Int -> Int
fatorialn n = foldr (*) 1 [1..n]

-- 2. Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos reais listados.

quadradoReal :: [Float] -> [Float]
quadradoReal xs = map (^2) xs

-- 3. Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de palavras e devolve uma lista com o comprimento de cada uma destas palavras.

comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras xs = map length xs

-- 4. Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior número entre 0 e 100000 que seja divisivel por 29.

maiorMultiploDe29 :: Int
maiorMultiploDe29 = maximum (filter (\x -> (mod x 29) == 0) [0..100000])

-- 5. Usando  a  função  filter  escreva  uma  função,  chamada  maiorMultiploDe que  recebe  um inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro.

maiorMultiploDe :: Int -> Int
maiorMultiploDe n = maximum (filter (\x -> (mod x n) == 0) [0..100000])

-- 6. Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠=12 +22 +32 +42...+𝑛2.

somaQuadrados :: Int -> Int
somaQuadrados n = foldr (+) 0 (map (^2) [1..n])

-- 7. Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o comprimento (cardinalidade) de uma lista dada.

comprimento :: [a] -> Int
comprimento xs = foldl (\acc x -> acc + 1) 0 xs

-- 8. Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. Para cada uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos.

-- flip

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

-- ord

ord' :: Char -> Int
ord' c = fromEnum c

-- max

max' :: Int -> Int -> Int
max' x y = if x > y then x else y

-- min

min' :: Int -> Int -> Int
min' x y = if x < y then x else y

-- curry

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \x y -> f (x, y)

-- uncurry

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(x, y) -> f x y

main = do
  putStr "fatorialn: entrada: 5; resultado: "
  print(fatorialn 5)
  putStr "quadradoReal: entrada: [1.0, 2.0, 3.0, 4.0, 5.0]; resultado: "
  print(quadradoReal [1.0, 2.0, 3.0, 4.0, 5.0])
  putStr "comprimentoPalavras: entrada: [\"Haskell\", \"is\", \"awesome\"]; resultado: "
  print(comprimentoPalavras ["Haskell", "is", "awesome"])
  putStr "maiorMultiploDe29: resultado: "
  print(maiorMultiploDe29)
  putStr "maiorMultiploDe: entrada: 29; resultado: "
  print(maiorMultiploDe 29)
  putStr "somaQuadrados: entrada: 5; resultado: "
  print(somaQuadrados 5)
  putStr "comprimento: entrada: [1, 2, 3, 4, 5]; resultado: "
  print(comprimento [1, 2, 3, 4, 5])
  putStr "flip: entrada: (+) 1 2; resultado: "
  print(flip' (+) 1 2)
  putStr "flip: entrada: (-) 1 2; resultado: "
  print(flip' (-) 1 2)
  putStr "ord: entrada: 'a'; resultado: "
  print(ord' 'a')
  putStr "ord: entrada: 'b'; resultado: "
  print(ord' 'b')
  putStr "max: entrada: 5 10; resultado: "
  print(max' 5 10)
  putStr "max: entrada: 54 5; resultado: "
  print(max' 54 5)
  putStr "min: entrada: 5 10; resultado: "
  print(min' 5 10)
  putStr "min: entrada: 54 4; resultado: "
  print(min' 54 4)
  putStr "curry: entrada: fst 1 2; resultado: "
  print(curry' fst 1 2)
  putStr "curry: entrada: fst 6 70; resultado: "
  print(curry' fst 6 70)
  putStr "uncurry: entrada: (+) (1, 2); resultado: "
  print(uncurry' (+) (1, 2))
  putStr "uncurry: entrada: (-) (1, 2); resultado: "
  print(uncurry' (-) (1, 2))
