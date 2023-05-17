--1--
data Rgb = Rgb { r :: Int
, g :: Int
, b :: Int
}

mezclar :: Rgb -> Rgb -> Rgb
mezclar (r1,g1,b1) (r2,g2,b2) = (div (r1 +r2)2, div (g1+g2)2, div (b2,b2)2)

--2--
type Linea = ([Char],Int)

vacia :: Linea
vacia = ([], 0)

moverIzq :: Linea -> Linea
moverIzq ([],0) = (xs, 0)
moverIzq (xs, n) = (xs, n-1)

moverDer :: Linea -> Linea
moverDer ([],_) = (xs,0)
moverDer (xs, n) = (xs, if n < length xs then n+1 else n)
o
moverDer :: Linea -> Linea
moverDer (xs,_) = (xs,0)
moverDer (xs, n) 
    | n < length xs = (xs, n+1) 
    | otherwise = (xs,n)

moverIni :: Linea -> Linea
moverIni (xs,0) = (xs,0)
moverIni (xs,_) = (xs, 0)

moverFin :: Linea -> Linea
moverFin ([],_) = vacia
moverFin (xs,_) = (xs, length xs)

insertar :: Char -> Linea -> Linea
insertar c (xs,n) = (ins n c xs, n+1)
ins 0 c xs = c:xs
ins n c (x:xs) = x: ins (n-1) c xs

borrar :: Linea -> Linea
borrar (xs,n) = (bor n xs, n-1 ) 
bor 0 xs = xs
bor 1 (x:xs) = xs
bor n (x:xs) = x :bor (n - 1) xs

--3--
--A--
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

l1 = Consnoc 1 (Consnoc 2 EmptyCL 3) 4
l2 = CUnit 1
l3 = EmptyCL


headCL :: CList a -> a
headCL (CUnit x) = x
headCL (Consnoc x xs y) = x

tailCL :: CList a -> CList a 
tailCL (CUnit x) = EmptyCL
tailCL (Consnoc x EmptyCL y) = CUnit y 
tailCL (Consnoc x xs y) = Consnoc (headCL xs) (tailCL xs) y  

isEmptyCL :: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnit ::  CList a -> Bool
isCUnit (CUnit _) = True
isCUnit _ = False

--B--
reverseCL :: CList a -> CList a
reverseCL (EmptyCL) = EmptyCL
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc x xs y) = Consnoc y (reverseCL xs) x

--C--


--D--

--E--

--4--
data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp
data Maybe int = Nothing | Just int

eval :: Aexp -> Int
eval (Num n) = n
eval (Prod a b) = eval a * eval b
eval (Div a b) = if b /= 0 then eval a `div` eval b else eval = 0



seval :: Aexp → Maybe Int
seval (Num n) = just n
seval (prod a b) = just (seval a * seval b)
seval (div a b) = Just (seval a ‘div‘ seval b)


--5--
--A--
data BST a = Empty | NODO (BST a) a (BST a) deriving Show
e1 = NODO (NODO Empty 5 Empty) 3 (NODO (NODO Empty 9 Empty) 7 (NODO Empty 5 Empty))

maximun :: Ord a => BST a -> a
maximun Empty = error "Arbol vacio"
maximun (NODO Empty r Empty) = r
maximun (NODO Empty r g)
    | r >= maximun g = r
    | otherwise = maximun g
maximun (NODO d r Empty)
    | r >= maximun d = r
    | otherwise = maximun d
maximun (NODO d r g)
    | (r >= maximun g) && (r >= maximun d) = r
    |  maximun g >= maximun d = maximun g
    |otherwise = maximun d
--B--
checkBST :: BST a -> Bool
checkBST Empty = False
checkBST (NODO _ r _) = True
