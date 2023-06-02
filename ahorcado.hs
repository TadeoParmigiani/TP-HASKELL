module Main where

import System.IO
ahorcado :: IO ()
ahorcado = do putStrLn  "piense una palabra"
              word <- sgetLine
              putStrLn "intente adivinarla"
              play word
sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                 do putChar '-'
                    xs <- sgetLine
                    return (x:xs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x
play :: String -> IO ()
play word = do putStr  ">"
               guess <- getLine
               if guess == word then
                  putStrLn "Ganaste!!"
                else
                    do putStrLn  (diff word guess)
                       play word

diff :: String -> String -> String
diff xs ys = [if elem x ys then x else '-' | x <- xs]

main = ahorcado
