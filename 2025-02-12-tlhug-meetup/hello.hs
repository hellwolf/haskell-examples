f a b = take b (repeat a)

main :: IO ()
main = print (take 10 (map (f 'h') [1 .. ]))

-- NOTE:
-- 1. hello world
-- 2. map function
-- 3. lazy evaluation
-- 4. lambda expression, and currying
-- 5. GHCI comand: :reload, :type, :info
-- 6. hoogle.haskell.org
-- 7. https://wiki.haskell.org/index.php?title=Typeclassopedia