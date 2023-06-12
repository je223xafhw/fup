getNonEmptyLine :: IO String
getNonEmptyLine = do
  input  <- getLine
  result <- if null input 
              then do putStrLn "Please enter a non-empty string."
                      getNonEmptyLine
              else return input
  return result

getNonEmptyLine2 :: IO String
getNonEmptyLine2 = getLine >>= 
    \input -> if null input then putStrLn "Please enter a non-empty string." >>= const getNonEmptyLine2 else return input