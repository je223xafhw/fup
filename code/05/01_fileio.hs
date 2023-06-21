import System.Directory
import System.IO
import Data.Char

hangman :: String -> IO Int
hangman s = hangman' s "" 0

hangman' :: String -> String -> Int -> IO Int
hangman' s d n = do
    putStrLn ("Word: " ++ s)
    putStr "Enter a char: "
    i <- toLower `fmap` getChar 
    let sec = decodeSecret s (i:d)
    putStrLn ""
    putStrLn ("Secret " ++ sec)
    if not (checkIfDone sec) then
        hangman' s (i:d) (if i `elem` d then n else n+1)
    else 
        return (n + 1)

decodeSecret :: String -> String -> String
decodeSecret ss xs = [if s `elem` xs then s else '*' | s<-ss]

checkIfDone :: String -> Bool
checkIfDone s = '*' `notElem` s


main = do
    let fileName = "woerterbuch.txt"
    let csvfileName = "ergebnisse.csv"
    filesExist <- mapM doesFileExist [fileName, csvfileName]
    if (False `elem` filesExist) then
        print "Es existieren nicht beide Dateien"
    else do
        putStr "Zahl eingeben: "
        input <-  getLine
        let parsed = reads input :: [(Int, String)]
        if null parsed then 
            print "Keine Zahl"
        else do
            let (num, _) = head parsed
            dict <- openFile fileName ReadMode
            contents <- hGetContents dict
            let singlewords = lines contents
            if (length singlewords < num) then
                print "Index zu gross"
            else do
                let word = fmap toLower (singlewords !! num)
                tries <- (hangman word)
                appendFile csvfileName (word ++ ", " ++ show tries ++ "\n")
                print tries
            hClose dict
