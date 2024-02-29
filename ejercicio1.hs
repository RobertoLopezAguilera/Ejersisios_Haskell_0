-- 1. Suma de elementos en una lista
sumar::[Int]->Int
sumar [ ] = 0
sumar (x:xs) = x + sumar(xs)

-- 2. Factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 3. Números pares
numerosPares :: Int -> [Int]
numerosPares n = [x | x <- [1..n], even x]

-- 4. Longitud de una cadena
longitudCadena :: String -> Int
longitudCadena [] = 0
longitudCadena (_:xs) = 1 + longitudCadena xs

-- 5. Reverso de una lista
reversoLista :: [a] -> [a]
reversoLista [] = []
reversoLista (x:xs) = reversoLista xs ++ [x]

-- 6. Duplicar elementos
duplicarElementos :: [Int] -> [Int]
duplicarElementos [] = []
duplicarElementos (x:xs) = x : x : duplicarElementos xs

-- 7. Filtrar elementos pares
filtrarPares :: [Int] -> [Int]
filtrarPares [] = []
filtrarPares (x:xs)
    | even x = x : filtrarPares xs
    | otherwise = filtrarPares xs

-- 8. Fibonacci
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- 9. Divisores de un número
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

-- 10. Palíndromo
esPalindromo :: String -> Bool
esPalindromo str = str == reverse str
