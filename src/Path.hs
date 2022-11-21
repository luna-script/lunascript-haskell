module Path where

import           AST
import           Compile
import qualified Data.Text.Internal.Lazy
import qualified Data.Text.Lazy.IO       as LIO
import           IR
import           Parser
import           Type
import           TypInf

doAllPath :: Data.Text.Internal.Lazy.Text -> IO ()
doAllPath str = do
  let (ast, varNames) = parseStmts str
  typedAst <- execTinfStmts ast varNames
  typedAst' <- mapM toSimpleTyped typedAst
  let (irStmts, env) = execConvertStmtsToIRStmts typedAst'
  LIO.putStrLn $ compileToLLVM irStmts env

showTypeCheck :: Data.Text.Internal.Lazy.Text -> IO ()
showTypeCheck str = do
  let (ast, varNames) = parseStmts str
  typedAst <- execTinfStmts ast varNames
  typedAst' <- mapM toSimpleTyped typedAst
  print typedAst'
