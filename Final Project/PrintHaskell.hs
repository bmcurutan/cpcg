module PrintHaskell where

--AST modules (internal)
import ASTFunctional as F
import ASTDesign as D

--Intermediary module (internal)
import qualified ToFunctional as T

--Helper doc module (internal)
import Helpers as H

--Implementation modules (internal)
import ImplQuicksort 
import ImplFactorial
import ImplExponent
import ImplGCDLCM 
import ImplFibonacci
import ImplPalindrome
import ImplMaximum
import ImplHello 

--External modules
import Text.PrettyPrint as P

--Print code using Haskell syntax
printHaskell :: Module -> Doc
printHaskell (Module nm imps fns) = text "module" <+> text nm <+> text "where" $$ impts imps $$ vcat (map build fns) 

--Print function body
build :: Function -> Doc 
build (Funct (FDecl _ nm paramlist) exs) = text nm <+> params paramlist <+> equals $+$ nest 4 (expr nm exs)
build FunctSkip = empty

--Print parameters (implicit types)
params :: [Declaration] -> Doc
params [] = empty
params (x:xs) = foldl1 (<+>) (map (\x -> pdecls x) (x:xs))
  where
    pdecls (PDecl (t,s) v) = text v

--Print imports/libraries
impts :: [String] -> Doc
impts [] = empty
impts (x:xs) = text "import" <+> text x $$ impts xs

--Print expressions
expr :: String -> Expr -> Doc
expr self (NodeExpr nd) = node self nd
expr self (IfExpr ex1 ex2 ex3) = text "if" <+> expr self ex1 $+$ nest 4 (text "then do" $+$ nest 4 (expr self ex2) $$ text "else do" $+$ nest 4 (expr self ex3))
expr _ Unit = unit
expr self (OperExpr o) = oper self o
expr self (SelfExpr t p) = callexpr self t self p
expr self (RevExpr nd) = text "reverse" <+> node self nd
expr self (LetExpr b ex) = letexpr self b $+$ nest 4 (expr self ex)
expr self (CallExpr t nm p) = callexpr self t nm p
expr self (MaxExpr nd1 nd2) = text "max" <+> node self nd1 <+> node self nd2
expr self (AsstExpr nd ex) = node self nd <+> text "<-" <+> expr self ex
expr self (CommExpr c) = comments c
expr self (OutExpr nd) = text "putStrLn" <+> node self nd 
expr self (ConcatExpr c) = concatexpr self c
expr self (Exprs ex1 ex2) = expr self ex1 $$ expr self ex2
expr self (PairExpr (ex1,ex2)) = parens $ expr self ex1 <^> expr self ex2
expr self (ReturnExpr s ex) = retexpr self s ex
expr self (PartExpr p i) = text "partition" <+> parens (less <+> node self p) <+> node self i
expr self (ArrExpr (StartIndex,EndIndex nd)) = integer 0 <+> parens (parens minus <+> node self nd <+> integer 1)
expr self (NullExpr nd) = text "null" <+> node self nd

--Print returns - sometimes implicit, sometimes not (e.g., mutable arrays)
retexpr :: String -> Struct -> Expr -> Doc
retexpr self ArrStruct (NodeExpr nd) = text "return" <> parens empty
retexpr self _ (NodeExpr nd) = node self nd
retexpr self _ ex = expr self ex --implicit

--Format function calls (built-in or not)
callexpr :: String -> Type -> [Char] -> [Expr] -> Doc
callexpr self t "concat" p = text "concat" <+> brackets (args self p)--use Haskell's built-in "concat" (different from ++)
  where args self p = foldl1 (<^>) $ map (\x -> expr self x) p
callexpr self PairType nm (l:r:ps) = text nm <+> args self ps 
callexpr self t nm p = text nm <+> args self p

--Print arguments, since some are operations
args :: String -> [Expr] -> Doc
args self [] = empty
args self p = foldl1 (<+>) $ map (\x -> expr self x) p

--Format and print bindings
letexpr :: String -> (Binding,Expr) -> Doc
letexpr self (BVar _ v,NodeExpr (Sel nd (ArrGet i))) = text "do" $+$ nest 4 (text v <+> text "<-" <+> node self (Sel nd (ArrGet i)))
letexpr self (BVar _ v, ex1) = text "let" <+> text v <+> equals <+> expr self ex1 <+> text "in do"
letexpr self (BPair (v1,v2), ex1) = text "let" <+> parens (text v1 <^> text v2) <+> equals <+> expr self ex1 <+> text "in do" 

--Print concatenation (built-in libraries)
concatexpr :: String -> Concatenation -> Doc
concatexpr self (F.List nd ex) = parens $ node self nd <> colon <> expr self ex
concatexpr self (F.Concat ex1 ex2) = expr self ex1 <+> H.conc <+> expr self ex2

--Print comment lines and blocks
comments :: Comments -> Doc
comments (CommLine c) = dbldash <+> text c
comments (CommBlock (x:xs)) = text "{-" <+> commblock (x:xs) <+> text "-}"

--Print each line on separate line
commblock :: [String] -> Doc
commblock [] = empty
commblock (x:xs) = text x $$ commblock xs

--Print nodes
node :: String -> Node -> Doc
node self (Var v) = text v
node self (Int i) = integer i
node self (F.Str s) = doubleQuotes $ text s
node self (Bool b) = if b then text "True" else text "False" 
node self (Sel nd p) = parens $ select self nd p
node self (Tup p) = args self p
node self Null = text "Null"

--Print selectors based on data structure and property
select :: String -> Node -> Property -> Doc
select self nd LsLength = text "length" <+> node self nd
select self nd LsHead = text "head" <+> node self nd
select self nd LsLast = text "last" <+> node self nd
select self nd LsTail = text "tail" <+> node self nd
select self nd LsInit = text "init" <+> node self nd
select self nd (LsAdd (Int 0) v) = parens $ node self v <> colon <> node self nd
select self nd (LsAdd (Int (-1)) v) = parens $ node self nd <> colon <> node self v
select self nd (ArrGet i) =  text "readArray" <+> node self nd <+> node self i
select self nd (ArrSet i v) = text "writeArray" <+> node self nd <+> node self i <+> node self v 

--Print operations in prefix form
oper :: String -> Operations -> Doc
oper self (Less nd1 nd2) = parens less <+> node self nd1 <+> node self nd2
oper self (Greater nd1 nd2) = parens greater <+> node self nd1 <+> node self nd2
oper self (Minus nd1 nd2) = parens $ parens minus <+> node self nd1 <+> node self nd2
oper self (Plus nd1 nd2) = parens $ parens plus <+> node self nd1 <+> node self nd2
oper self (Times nd1 nd2) = parens $ parens times <+> node self nd1 <+> node self nd2
oper self (Divide nd1 nd2) = parens $ text "div" <+> node self nd1 <+> node self nd2
oper self (Mod nd1 nd2) = parens $ text "mod" <+> node self nd1 <+> node self nd2
oper self (Equals nd1 nd2) = parens eqequals <+> node self nd1 <+> node self nd2
oper self (NotEqual nd1 nd2) = parens (slash <> equals) <+> node self nd1 <+> node self nd2
oper self (NotLess nd1 nd2) = parens notless <+> node self nd1 <+> node self nd2
oper self (LessEqual nd1 nd2) = parens lessequal <+> node self nd1 <+> node self nd2
oper self (F.Mid nd1 nd2) = node self nd1 <+> plus <+> parens (node self nd2 <+> minus <+> node self nd1) <> divide <> integer 2
oper self (And ex1 ex2) = parens andd <+> parens (expr self ex1) <+> parens (expr self ex2)
oper self (Or ex1 ex2) = parens orr <+> parens (expr self ex1) <+> parens (expr self ex2)
oper self (Not ex) = text "not" <+> parens (expr self ex)

--used in Main to generate code
code :: [Char] -> [D.Choices] -> Doc
code "quicksort" ch = printHaskell $ T.toFunct $ qsProg ch
code "fibonacci" ch = printHaskell $ T.toFunct $ fibProg ch
code "factorial" ch = printHaskell $ T.toFunct $ facProg ch
code "exponent" ch = printHaskell $ T.toFunct $ expProg ch
code "gcdlcm" ch = printHaskell $ T.toFunct $ glProg ch 
code "palindrome" ch = printHaskell $ T.toFunct $ palProg ch
code "maximum" ch = printHaskell $ T.toFunct $ maxProg ch
code "hello" ch = printHaskell $ T.toFunct $ helloProg ch
