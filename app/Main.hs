module Main (main) where

import           Compile
import           Data.Text.Lazy.IO as LT
import           Parser
import           TypInf

main :: IO ()
main = do
    str <- LT.getContents
    let ast = parseStmts str
    tenv <- execTinfStmts ast
    LT.putStrLn $ compileToLLVM ast
