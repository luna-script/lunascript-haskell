{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE RecursiveDo #-}
module Compile where

import           AST
import           Control.Monad.Fix
import           Control.Monad.Trans.State
import qualified Data.Map                   as M
import           Data.String.Transform
import           Data.Text
import qualified Data.Text.Lazy
import           LLVM.AST                   hiding (function, value)
import           LLVM.AST.Constant          as ASTCons
import           LLVM.AST.IntegerPredicate  as P
import           LLVM.AST.Type              as ASTType
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import           LLVM.Pretty

type Env = M.Map Text Operand

compileExpr :: (MonadFix m, MonadIRBuilder m) => Expr -> StateT Env m Operand
compileExpr (EInt n) = pure $ int32 n
compileExpr (BinOp op e1 e2) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    text2operand op e1' e2'
compileExpr (Var name) = do
    env <- get
    let opr = case M.lookup name env of
            Just x  -> x
            Nothing -> error $ "variable " ++ show name ++ " not found"
    call opr []
compileExpr (EIf condExpr thenExpr elseExpr) = mdo
    cond <- compileExpr condExpr
    condBr cond ifThen ifElse
    ifThen <- block `named` "then"
    oprThen <- compileExpr thenExpr
    br ifEnd
    endOfThen <- currentBlock
    ifElse <- block `named` "else"
    oprElse <- compileExpr elseExpr
    br ifEnd
    endOfElse <- currentBlock
    ifEnd <- block `named` "end"

    phi [(oprThen, endOfThen), (oprElse, endOfElse)]

compileStmt :: (MonadFix m, MonadModuleBuilder m) => Stmt -> StateT Env m Operand
compileStmt (TopLevelLet name e) = do
    env <- get
    e' <- function (Name $ toShortByteString name) [] i32 $ \[] -> do
        val <- evalStateT (compileExpr e) env
        ret val
    let env' = M.insert name e' env
    put env'
    pure e'

compileStmts :: (MonadFix m, MonadModuleBuilder m) => [Stmt] -> StateT Env m Operand
compileStmts [] = error "main is required"
compileStmts [s] = compileStmt s
compileStmts (s:ss) = do
    compileStmt s
    compileStmts ss

text2operand :: (MonadIRBuilder m) => Text -> Operand -> Operand -> m Operand
text2operand "+" = add
text2operand "-" = sub
text2operand "*" = mul
text2operand "/" = sdiv
text2operand "<" = icmp P.SLT
text2operand _   = error "unimpremented!"

compileToLLVM :: [Stmt] -> Data.Text.Lazy.Text
compileToLLVM ast =
    ppllvm $ buildModule "main" $ do
        evalStateT (compileStmts ast) M.empty
