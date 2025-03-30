nome :: IO ()
nome = do
  putStrLn "Digite seu nome:"
  name <- getLine
  putStrLn ("Olá, " ++ name ++ ", como você está?")
