module Main where

import Lexer 
import Parser
import TypeChecker
import Interpreter

main = getContents >>= putStrLn . printPretty . eval . typecheck . parser . lexer 