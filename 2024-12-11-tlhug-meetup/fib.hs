fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- fib' n = case n of
--      0 -> 0
--      1 -> 1
--      n -> fib' (n - 1) + fib' (n - 2)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

main = do
    -- print (take 10 (fmap fib [1..]))
    -- print $ take 10 $ fmap fib [1..]
    print $ fibs !! 100
