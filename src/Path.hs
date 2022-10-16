module Path where

import           Compile
import qualified Data.Text.Internal.Lazy
import qualified Data.Text.Lazy.IO       as LIO
import           IR
import           Parser
import           TypInf

doAllPath :: Data.Text.Internal.Lazy.Text -> IO ()
doAllPath str = do
    let ast = parseStmts str
    tenv <- execTinfStmts ast
    tenv' <- mapM convertTypToTyp' tenv
    let irStmts = execConvertStmtsToIRStmts tenv' ast
    LIO.putStrLn $ compileToLLVM irStmts
