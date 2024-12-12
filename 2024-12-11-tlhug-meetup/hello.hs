wrapStr s = "[" ++ s ++ "]" 

wrapStrWith :: String -> (String, String) -> String
wrapStrWith s (start, end) = start ++ s ++ end

main = do
    let expr = wrapStrWith "Hello, world"
    putStrLn (expr ("{", "}"))
    putStrLn (expr ("«", "»"))