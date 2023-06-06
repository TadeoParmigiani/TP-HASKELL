import System.IO

ahorcado :: IO()
ahorcado = do putStrLn "Piense una palabra"
              word <- sgetLine
              putStrLn "Intente adivinar"
              play word 5

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else
                  do putChar '-'
                     xs <- sgetLine
                     return (x:xs)

getCh ::  IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

play :: String -> Int -> IO ()
play word n = do putStr ("Intento"++ show n ++">")
                 guess <- getLine
                 if length word > 1 then
                   if guess == word then putStrLn "Ganaste"
                   else putStrLn "Perdiste"
                 else
                    if n == 0 then putStrLn "Perdiste"
                    else if (elem(head guess)word) then
                                                    play word n guess ++ac do putStrLn (diff word guess)
                                                    play word (n -1)


diff :: String -> String -> String
diff xs ys = [if elem x ys then x else '-' | x <- xs]


main = ahorcado
