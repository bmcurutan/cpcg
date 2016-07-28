# CPCG: A Cross-Paradigm Code Generator

## M.Eng. Software Development Project 

- Analyzed standard programs in cross-paradigm languages to develop an expressive design language (DSL)
- Constructed this Haskell code generator to demonstrate the existence of abstract algorithms, a language of design, and a shared core across paradigms
- Generated programming languages: Java, C, Lambda-Prolog, Haskell, Lua, Lisp, C#, and Scala

--- 

## Abstract

The goal of this project is to demonstrate the existence of a language of design that bridges the gap between abstract algorithms and explicit code. The design decisions made between the specifications of abstract algorithms and their concrete implementations may be encoded in a code generator using this
tangible language of design. The Cross-Paradigm Code Generator (CPCG) is designed as a tool to generate code, allowing programmers to solve problems at the level of abstraction of the problem domain. Two domain specific languages were developed to incorporate the core ideas of this project; that is, the concepts and semantic characteristics of all the paradigms and the choices behind the design of algorithms. The generated code demonstrates the existence of abstract algorithms and a tangible language of design.

---

## Getting Started (on Mac)

1. Install [Haskell](https://www.haskell.org/platform/)
2. Open a terminal and type `ghci` for the GHC Interpreter
3. Change to the project directory with `:cd` command
4. Load the modules using `:l Main`
5. Use the examples provided (`ex1, ex2,...`) or call the `generate` function with new parameters

### The `generate` Function
`generate :: [Char] -> [Char] -> [D.Choices] -> IO ()`

- The first parameter (string) specifies the folder path in which the source file will generate
- The second parameter (string) specifies the algorithm to generate; currently, the only ones available are: quicksort, fibonacci, factorial, exponent, palindrome, greatest common divisor (gcd) and least common multiple (lcm), maximum, and hello
- The third parameter (array) specifies the design decisions for the algorithm, including but not limited to: pivot choice (head or last), loop (iterative or recursive), language, and paradigm
- The resulting code is generated in a source file in the path from the first parameter, or into the same folder as `Main.hs` if no path is provided