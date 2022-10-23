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
    let ast = parseStmts str
    typedAst <- execTinfStmts ast
    typedAst' <- mapM convertStmtTypedToSimpleTyped typedAst
    let irStmts = execConvertStmtsToIRStmts typedAst'
    LIO.putStrLn $ compileToLLVM irStmts
