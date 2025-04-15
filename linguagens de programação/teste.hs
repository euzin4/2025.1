fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)
    
nome :: IO ()
nome = do
  putStrLn "Digite seu nome:"
  name <- getLine
  putStrLn ("Olá, " ++ name ++ ", como você está?")

menor x | x < 5 = "menor"
        | x == 5 = "igual"
        | otherwise = "maior"

imenor x = if x < 5 then "menor" else "maior"

media :: Fractional a => [a] -> Maybe a
media [a,b,c] = Just((a+b+c)/3)

