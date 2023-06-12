hangman :: String -> IO ()
hangman s = hangman' s "" 0

hangman' :: String -> String -> Int -> IO ()
hangman' s d n = do
    putStr "Enter a char: "
    i <- getChar
    let sec = decodeSecret s (i:d)
    putStrLn ""
    putStrLn ("Secret " ++ sec)
    if not (checkIfDone sec) then
        hangman' s (i:d) (if i `elem` d then n else n+1)
    else 
        putStrLn ("Solved in " ++ show (n + 1) ++ " tries")


decodeSecret :: String -> String -> String
decodeSecret ss xs = [if s `elem` xs then s else '*' | s<-ss]

checkIfDone :: String -> Bool
checkIfDone s = '*' `notElem` s

main :: IO ()
main = do
    hangman "hallo"