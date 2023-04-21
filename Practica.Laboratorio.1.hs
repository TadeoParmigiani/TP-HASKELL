module Practica0 where

import Data.List

{-
1) Los siguientes códigos tienen errores, cargar el archivo 20.Practica.0.hs en el interprete de Haskell
GHCi, leer los mensajes de error y corregirlos hasta que el archivo se cargue correctamente.
-}

-- a)
regla b = case b of
    True -> "Quedate en Casa"
    False -> "Qudate en Casa"

-- b)
case1 [x]         =  []
case1 (x:y:xs)    =  y : case1 (x:xs)
case1 []          =  []

-- c)
mimap f []        =  []
mimap f (x:xs)     =  f x : mimap (f) xs

-- d)
listNumeros = 1 : (2 : (3 : []))

-- e)
[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys

-- f)
addToTail (x:xs) = map (+x) (tail xs)

-- g)
listmin :: [a1] -> a2 -> c
listmin xs = head . sort $ xs
-- h) (*)
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs

{-
2. Definir las siguientes funciones y determinar su tipo:

a) five, que dado cualquier valor, devuelve 5
    five :: Num t => t1 -> t
    five n = 5
    main = putStrLn "Hello, World!
b) apply, que toma una función y un valor, y devuelve el resultado de
aplicar la función al valor dado
    apply :: (t1 -> t2) -> t1 -> t2
    apply f x = (f x) + 2
    main = putStrLn "Hello, World!"
c) identidad, la función identidad
    identidad :: int -> int
    identidad x = x
    main = putStrLn "Hello, World!"

d) first, que toma un par ordenado, y devuelve su primera componente
    first :: (t1, t) -> t1
    first (x, _) = x

e) derive, que aproxima la derivada de una función dada en un punto dado

f) sign, la función signo
sign x | x < 0  = -1
       | x == 0 = 0
       | True  = 1
g) vabs, la función valor absoluto (usando sign y sin usarla)
vabs :: (Ord t, Num t) => t -> t
vabs n | n < 0 = negate n
       | True  = n

vabs' :: (Ord t, Num t) => t -> t
vabs' n = n * sign n

h) pot, que toma un entero y un número, y devuelve el resultado de
elevar el segundo a la potencia dada por el primero
pot :: Int -> Int -> Int 
pot x y = y^x 

i) xor, el operador de disyunción exclusiva
 xor :: bool -> bool -> bool
 xor q p | q==true && p==true = false
         | q==true && p==false = true
         | q==false && p==true = true
         | q==false && p==false = false
j) max3, que toma tres números enteros y devuelve el máximo entre llos
 max3 :: int -> int -> int -> int
 max3 a b c = | a>b && a>c =a  
              | b>a && b>c =b  
              | c>a && c>b =c               

k) swap, que toma un par y devuelve el par con sus componentes invertidas
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)
-}

{-
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?

-}

{-
4) Dar al menos dos ejemplos de funciones que tengan cada uno de los siguientes tipos:
a) (Int -> Int) -> Int
pep x y = x + y

b) Int -> (Int -> Int)
draw n = n + 1
c) (Int -> Int) -> (Int -> Int)

fooc f n = (f (n+1)*1)

d) Int -> Bool
food n = n > 4
e) Bool -> (Bool -> Bool)
fooe n = if (even n) && n > 10

f) (Int,Char) -> Bool

foof :: (Int,Char) -> Bool
foof (n,x) = n `elem` [0..10] && x `elem` ['a'..'z']

g) (Int,Int) -> Int

foog :: (Int,Int) -> Int
foog (x,y) = fst (x,y)

h) Int -> (Int,Int)
fooh ::  Int -> (Int,Int)
fooh x = (x+1,x+2)

i) a -> Bool

j) a -> a
-}


{-
5) Definir las siguientes funciones usando listas por comprensión:

a) 'divisors', que dado un entero positivo 'x' devuelve la
lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)
-}
divisors :: Int -> [Int]
divisors x
  | x <= 0    = []
  | otherwise = filter (\n -> x `mod` n == 0) [1..x]
{-
b) 'matches', que dados un entero 'x' y una lista de enteros descarta
de la lista los elementos distintos a 'x'
-}
matches :: Int -> [Int] -> [Int]
matches x = filter (==x)
{-
c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
donde 0 <= a, b, c, d <= 'n'
-}
cuadrupla :: int -> [int]
cuadrupla n = 
{-
(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
'xs' sin elementos repetidos
-}
unique :: [Int] -> [Int]
unique [] = []
unique (x:xs) = x : unique (filter (/= x) xs)



{-
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. -}

{-
7) Sin usar funciones definidas en el
preludio, defina recursivamente las siguientes funciones y
determine su tipo más general:

a) 'suma', que suma todos los elementos de una lista de números

b) 'alguno', que devuelve True si algún elemento de una
lista de valores booleanos es True, y False en caso
contrario

c) 'todos', que devuelve True si todos los elementos de
una lista de valores booleanos son True, y False en caso
contrario

d) 'codes', que dada una lista de caracteres, devuelve la
lista de sus ordinales

e) 'restos', que calcula la lista de los restos de la
división de los elementos de una lista de números dada por otro
número dado

f) 'cuadrados', que dada una lista de números, devuelva la
lista de sus cuadrados

g) 'longitudes', que dada una lista de listas, devuelve la
lista de sus longitudes

h) 'orden', que dada una lista de pares de números, devuelve
la lista de aquellos pares en los que la primera componente es
menor que el triple de la segunda

i) 'pares', que dada una lista de enteros, devuelve la lista
de los elementos pares

j) 'letras', que dada una lista de caracteres, devuelve la
lista de aquellos que son letras (minúsculas o mayúsculas)

k) 'masDe', que dada una lista de listas 'xss' y un
número 'n', devuelve la lista de aquellas listas de 'xss'
con longitud mayor que 'n' -}

{-
8) Redefinir las funciones del ejercicio anterior usando foldr, map y filter.
ver su definición en https://hoogle.haskell.org/
-}
