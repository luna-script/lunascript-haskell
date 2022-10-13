{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE TemplateHaskell        #-}
module Compile where

import           AST
import           Control.Lens
import           Control.Monad.Fix
import           Control.Monad.Trans.State
import           Data.Foldable              as F
import qualified Data.Map                   as M
import           Data.String.Transform
import           Data.Text
import qualified Data.Text.Lazy
import           LLVM.AST                   hiding (function, value)
import           LLVM.AST.AddrSpace
import           LLVM.AST.Constant          as ASTCons
import           LLVM.AST.IntegerPredicate  as P
import           LLVM.AST.Type              as ASTType
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import           LLVM.Pretty

data Env = Env {
        _envGlobalConstant :: M.Map Text Operand,
        _envEnv            :: M.Map Text Operand
    }

makeFields ''Env

compileExpr :: (MonadFix m, MonadIRBuilder m) => Expr -> StateT Env m Operand
compileExpr (EInt n) = pure $ int32 n
compileExpr (BinOp op e1 e2) = do
    e1' <- compileExpr e1
    e2' <- compileExpr e2
    text2operand op e1' e2'
compileExpr (Var name) = do
    constantEnv <- use globalConstant
    functionEnv <- use env
    case M.lookup name constantEnv of
        Just x  -> call x []
        Nothing -> case M.lookup name functionEnv of
            Just f  -> pure f
            Nothing -> error $ "variable " ++ toString name ++ "is undefined"
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
compileExpr (Fun _ _) = error "compile Fun unimpremented"
compileExpr (EThunk _) = error "compile Thunk unimpremented"
compileExpr (ExecThunk e) = do
    e' <- compileExpr e
    call e' []
compileExpr (FunApp e1 e2) = do
    let (funExpr, argsExpr) = separateFunArgs e1 e2
    fun <- compileExpr funExpr
    args <- mapM compileExpr argsExpr
    call fun (Prelude.zip args (repeat []))


separateFunArgs :: Expr -> Expr -> (Expr, [Expr])
separateFunArgs (FunApp e11 e21) e2 = let
    (fun, args) = separateFunArgs e11 e21
    in (fun, args ++ [e2])
separateFunArgs e1 e2 = (e1, [e2])

compileStmt :: (MonadFix m, MonadModuleBuilder m) => Stmt -> StateT Env m Operand
compileStmt (TopLevelLet name (Fun arg e)) = do
    let (args, e') = collectArgs (Fun arg e)
    constantEnv <- use globalConstant
    functionEnv <- use env
    let typ = FunctionType i32 (Prelude.replicate (F.length args) i32) False
        ptrTyp = ASTType.PointerType typ (AddrSpace 0)
        ref = ASTCons.GlobalReference ptrTyp (Name $ toShortByteString name)
        functionEnv' = M.insert name (ConstantOperand ref) functionEnv
    env .= functionEnv'
    function (Name $ toShortByteString name) (Prelude.zip (repeat i32) (Prelude.map ( ParameterName . toShortByteString ) args)) i32 $ \oprs -> do
        let newEnv = F.foldr (\(k,v) env -> M.insert k v env) functionEnv' (Prelude.zip args oprs)
        opr <- evalStateT (compileExpr e') (Env constantEnv newEnv)
        ret opr
compileStmt (TopLevelLet name (EThunk e)) = do
    env' <- get
    e' <- function (Name $ toShortByteString name) [] i32 $ \[] -> do
        val <- evalStateT (compileExpr e) env'
        ret val
    env .= M.insert name e' (env'^.env)
    pure e'

compileStmt (TopLevelLet name e) = do
    env <- get
    e' <- function (Name $ toShortByteString name) [] i32 $ \[] -> do
        val <- evalStateT (compileExpr e) env
        ret val
    globalConstant .= M.insert name e' (env^.globalConstant)
    pure e'

collectArgs :: Expr -> ([Text], Expr)
collectArgs (Fun arg e) = let
    (args, e') = collectArgs e
    in (arg:args, e')
collectArgs e = ([], e)

compileStmts :: (MonadFix m, MonadModuleBuilder m) => [Stmt] -> StateT Env m Operand
compileStmts [] = error "main is required"
compileStmts [s] = compileStmt s
compileStmts (s:ss) = do
    compileStmt s
    compileStmts ss

text2operand :: (MonadIRBuilder m) => Text -> Operand -> Operand -> m Operand
text2operand "+"  = add
text2operand "-"  = sub
text2operand "*"  = mul
text2operand "/"  = sdiv
text2operand "<"  = icmp P.SLT
text2operand "==" = icmp P.EQ
text2operand _    = error "unimpremented!"

compileToLLVM :: [Stmt] -> Data.Text.Lazy.Text
compileToLLVM ast =
    ppllvm $ buildModule "main" $ do
        evalStateT (compileStmts ast) (Env M.empty M.empty)
