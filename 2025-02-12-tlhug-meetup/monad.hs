countLetterH :: String -> Int
countLetterH = length . filter (== 'h')
-- countLetterH s = length $ filter (== 'h') s

-- main = do
--     s <- getLine
--     print $ countLetterH s

f :: Maybe String -> Maybe Int
f s = do
    (s' :: String) <- s
    pure (countLetterH s')

main =
    -- (>>=) :: IO String -> (String -> IO b) -> IO b
    -- (>>=) getLine (print . countLetterH)
    -- (>>=) getLine (print . countLetterH)
    print . countLetterH =<< getLine
