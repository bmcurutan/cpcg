module ASTDesign where

--External modules
import System.IO
import Control.Monad
import Text.PrettyPrint

--Imports for generated code to compile and run
type Library = String

--Different categories of design choices available
--Not all categories are used in all algorithsm (e.g., no pivots in factorial)
data Choices = Piv Pivot
             | Str Structure
             | Loop Loop
             | Lib Libraries
             | Lang Language
             | Para Paradigm
  deriving Eq

--Pivot choices (mid is not available in lists)
data Pivot = Head 
           | Mid 
           | Last 
  deriving Eq

--Structure choices 
data Structure = None 
               | List 
               | Array 
  deriving Eq

--Repetition choices, recursion and iteration (e.g., while loops)
data Loop = Rec 
          | Iter 
  deriving Eq

--Library choices (do not need to generate if selected), partition and concatenation
data Libraries = Part
               | Concat --use ++ instead of concat
  deriving Eq

--Language choices
data Language = Haskell 
              | Java
              | C
              | LP --lambda-prolog
              | Prolog
              | CSharp
              | Scala
              | Lua
              | Lisp
              | ObjectiveC
  deriving Eq

--Paradigm choices - implicit for all languages except Scala
data Paradigm = Func --functional
              | Log --logic
              | OO --object-oriented
              | Imp --imperative
  deriving Eq

