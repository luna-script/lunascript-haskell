module Pass (doAllPass, showTypeCheck, showAlpha) where

import           Alpha
import           AST
import           Compile
import qualified Data.Text.Internal.Lazy
import qualified Data.Text.Lazy.IO       as LIO
import           IR
import           Parser
import           TypInf

doAllPass :: Data.Text.Internal.Lazy.Text -> IO ()
doAllPass str = do
  let (ast, varNames) = parseStmts str
  typedAst <- execTinfStmts ast varNames
  typedAst' <- mapM toSimpleTyped typedAst
  let typedAst'' = alpha varNames typedAst'
  let (irStmts, env) = execConvertStmtsToIRStmts typedAst''
  LIO.putStrLn $ compileToLLVM irStmts env

showTypeCheck :: Data.Text.Internal.Lazy.Text -> IO ()
showTypeCheck str = do
  let (ast, varNames) = parseStmts str
  typedAst <- execTinfStmts ast varNames
  typedAst' <- mapM toSimpleTyped typedAst
  print typedAst'

showAlpha :: Data.Text.Internal.Lazy.Text -> IO ()
showAlpha str = do
  let (ast, varNames) = parseStmts str
  typedAst <- execTinfStmts ast varNames
  typedAst' <- mapM toSimpleTyped typedAst
  let typedAst'' = alpha varNames typedAst'
  print typedAst''
