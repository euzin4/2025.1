mul :: Int -> Int -> Int
mul m n
    | n == 0 = 0
    | n > 0  = m + mul m(n-1)