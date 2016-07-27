module Hello where
hello :: IO()
hello = 
    putStrLn "Hello World!" {- do
  putStrLn $ "Name?"
  name <- getLine
  putStrLn $ "Hello, " ++ name --also need print (not just printLn) -}

