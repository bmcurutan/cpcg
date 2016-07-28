module Main where

--ASTs (internal)
import ASTDesign as D

--Pretty printers (internal)
import PrintHaskell as H
import PrintJava as J
import PrintC as C
import PrintLambdaProlog as LP
import PrintCSharp as CS
import PrintLisp as L
import PrintLua as Lu
import PrintScala as S

--External modules
import System.IO
import Text.PrettyPrint.HughesPJ 

--Main function to print source code in an external file
--File extension automatically determined based on programming language
--Algorithm name (alg) and design choices, including language, must match project code exactly 
generate :: [Char] -> [Char] -> [D.Choices] -> IO ()
generate path alg ch = 
    if (elem (D.Lang Haskell) ch)
        then do outh <- openFile (path ++ alg ++ ".hs") WriteMode
                hPutStrLn outh $ render $ H.code alg ch
                hClose outh  
        else if (elem (D.Lang Java) ch)
            then do outh <- openFile (path ++ alg ++ ".java") WriteMode
                    hPutStrLn outh $ render $ J.code alg ch
                    hClose outh  
            else if (elem (D.Lang C) ch) 
                then do outh <- openFile (path ++ alg ++ ".c") WriteMode
                        hPutStrLn outh $ render $ C.code alg ch
                        hClose outh  
                else if (elem (D.Lang CSharp) ch) 
                    then do outh <- openFile (path ++ alg ++ ".cs") WriteMode
                            hPutStrLn outh $ render $ CS.code alg ch
                            hClose outh  
                    else if (elem (D.Lang LP) ch || elem (D.Lang Prolog) ch) 
                        then do outh <- openFile (path ++ alg ++ ".pl") WriteMode
                                hPutStrLn outh $ render $ LP.code alg ch
                                hClose outh  
                        else if (elem (D.Lang Lisp) ch) 
                            then do outh <- openFile (path ++ alg ++ ".lsp") WriteMode
                                    hPutStrLn outh $ render $ L.code alg ch
                                    hClose outh  
                            else if (elem (D.Lang Lua) ch) 
                                then do outh <- openFile (path ++ alg ++ ".lua") WriteMode
                                        hPutStrLn outh $ render $ Lu.code alg ch
                                        hClose outh  
                                else if (elem (D.Lang Scala) ch) 
                                    then do outh <- openFile (path ++ alg ++ ".scala") WriteMode
                                            hPutStrLn outh $ render $ S.code alg ch
                                            hClose outh  
                                    else if (elem (D.Lang ObjectiveC) ch)
                                        then do outh <- openFile (path ++ alg ++ ".m") WriteMode
                                                hPutStrLn outh $ render $ S.code alg ch
                                                hClose outh  
                                        else error "Invalid parameters."  

--Example tests for various languages, algorithms, and design decisions
--Empty path -> code is generated in same folder as Main module
ex1 = generate "" "quicksort" [D.Piv Last, D.Str List, D.Loop Rec, D.Lib Part, D.Lib Concat, D.Lang Haskell]
ex2 = generate "" "quicksort" [D.Piv Head, D.Str Array, D.Loop Iter, D.Lang CSharp]
ex3 = generate "" "fibonacci" [D.Loop Iter, D.Lang Scala, D.Para OO]
ex4 = generate "" "factorial" [D.Loop Rec, D.Lang Lua]
ex5 = generate "" "maximum" [D.Lang LP]
ex6 = generate "" "palindrome" [D.Lang CSharp]
ex7 = generate "" "hello" [D.Lang Scala, D.Para Func]
ex8 = generate "" "quicksort" [D.Piv Mid, D.Str Array, D.Lang Java, D.Loop Iter]

main :: IO ()
main = return ()