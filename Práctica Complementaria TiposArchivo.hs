data Nat = Cero | Succ Nat deriving Show
--Succ :: Nat -> Nat
--b--
int2Nat :: Int -> Nat
int2Nat 0 = Cero
int2Nat n = Succ (int2Nat (n - 1))
--c--
suma :: Nat -> Nat -> Nat
suma Cero x = x
suma n Cero = n
suma (Succ n) x = Succ (suma x n)
--d--
nat2Int :: Nat -> Int
nat2Int Cero = 0
nat2Int (Succ n) = 1 + nat2Int n
--2--
--a--
data Arb = E | H Int | N Arb Arb deriving Show
data Cmd = L | R deriving Eq

--N :: Arb -> Arb -> Arb--
--b--
selec::[Cmd] -> Arb -> Arb
selec [] t = t
selec (x:xs) (N l r) = case x of
                        L -> selec xs l
                        R -> selec xs r
--c--
enum :: Arb -> [[Cmd]]
enum E = [[]]
enum (H _) = [[]]
enum (N a b) = map (L:) (enum a) ++ map (R:) (enum b)

--3--
type Nombre = String
type A = Int
type Estado a = [(Nombre, A)]
inicial :: Estado a
inicial = []

update :: Nombre -> A -> Estado a -> Estado a
update nombre valor [] =  [(nombre, valor)]
update nombre valor ((n, v): xs)
                          | n == nombre = (nombre, valor) : xs
                          | otherwise = (n, v) : update nombre valor xs

lookfor :: Nombre -> Estado a -> Maybe A
lookfor nombre ((n,v):xs) 
                  | n == nombre = Just v
                  | otherwise = Nothing

free :: Nombre -> Estado a -> Estado a
free _ [] = []
free nombre ((n,v): xs) 
                | nombre == n = xs
                | otherwise = free nombre ((n,v): xs)
--4--
data BTS a = H | N (BTS a) a (BTS a)
e1 = N (N H 2 H) 3 (N (N H 5 H) 7 (N H 9 H))
--a--
calcu _ H = 0
calcu n (N l a r) 
            | n  == 0 = 1
            | otherwise = calcu (n - 1) l + calcu (n - 1) r
--b--
altura H = 0
altura (N l x r) = 1 + max (altura l) (altura r)
balans (N r x l) = let n = altura r
                       m = altura l
                    in if (m - n) == 0 || (m - n) == 1 then True else False
