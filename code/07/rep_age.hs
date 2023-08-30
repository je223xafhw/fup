main :: IO ()
main = do 
    putStr "Alter: "
    age <- getLine
    if read age > 12 then
        putStrLn "if her age is on the clock..."
    else 
        putStrLn "?"

main2 :: IO ()
main2 = putStr "Alter: " >> getLine >>= \age -> if read age > 12 then putStrLn "if her age is on the clock..." else putStrLn "?"

main3 = putStr "Alter: " >> getLine >>= \age -> let alter = read age in (if alter > 12 then putStrLn "???" else putStrLn "!!!")