{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE TemplateHaskell        #-}
module Compile where

import           Control.Lens
import           Control.Monad.Fix
import           Control.Monad.State        (MonadState)
import           Control.Monad.Trans.State
import qualified Data.Bifunctor
import           Data.Foldable              as F
import qualified Data.Map                   as M
import           Data.String.Transform
import qualified Data.Text.Lazy
import           IR
import           LLVM.AST                   hiding (function, value)
import           LLVM.AST.AddrSpace
import           LLVM.AST.Constant          as ASTCons
import           LLVM.AST.Type              as ASTType
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import qualified LLVM.Prelude               as Pr
import           LLVM.Pretty

type GlobalContextMap = M.Map Pr.ShortByteString Operand
type EnvMap = M.Map Pr.ShortByteString Operand

data Env = Env {
        _envGlobalConstant :: GlobalContextMap,
        _envEnv            :: EnvMap
    }

makeFields ''Env

compileIRExpr :: (MonadState s m, MonadFix m, MonadIRBuilder m, HasGlobalConstant s GlobalContextMap, HasEnv s EnvMap) => IRExpr m -> m Operand
compileIRExpr (IRInt n) = pure $ int32 n
compileIRExpr (IRBool b) = pure $ if b then bit 1 else bit 0
compileIRExpr IRUnit = pure $ ConstantOperand $ Undef $ StructureType False []
compileIRExpr (IROp op e1 e2) = do
    e1' <- compileIRExpr e1
    e2' <- compileIRExpr e2
    op e1' e2'
compileIRExpr (IRIf condExpr thenExpr elseExpr) = mdo
    cond <- compileIRExpr condExpr
    condBr cond ifThen ifElse
    ifThen <- block `named` "then"
    oprThen <- compileIRExpr thenExpr
    br ifEnd
    endOfThen <- currentBlock
    ifElse <- block `named` "else"
    oprElse <- compileIRExpr elseExpr
    br ifEnd
    endOfElse <- currentBlock
    ifEnd <- block `named` "end"

    phi [(oprThen, endOfThen), (oprElse, endOfElse)]
compileIRExpr (IRVar name) = do
    constantEnv <- use globalConstant
    functionEnv <- use env
    case M.lookup name constantEnv of
        Just x  -> call x []
        Nothing -> case M.lookup name functionEnv of
            Just f  -> pure f
            Nothing -> error $ "variable " ++ toString name ++ "is undefined"
compileIRExpr (IRExecThunk e) = do
    e' <- compileIRExpr e
    call e' []
compileIRExpr (IRFunApp e1 oprs) = do
    e1' <- compileIRExpr e1
    oprs' <- mapM compileIRExpr oprs
    call e1' (Prelude.zip oprs' (repeat []))

compileIRStmt :: (MonadModuleBuilder m, MonadFix m) => IRStmt (StateT Env (IRBuilderT (StateT Env m)))-> StateT Env m Operand
compileIRStmt (TopLevelConst var t e) = do
    env' <- get
    e' <- function (Name var) [] t $ \[] -> do
        val <- evalStateT (compileIRExpr e) env'
        ret val
    globalConstant .= M.insert var e' (env'^.globalConstant)
    pure e'
compileIRStmt (TopLevelThunkDef name t e) = do
    env' <- get
    e' <- function (Name name) [] t $ \[] -> do
        val <- evalStateT (compileIRExpr e) env'
        ret val
    env .= M.insert name e' (env'^.env)
    pure e'
compileIRStmt (TopLevelFunDef name args returnType body) = do
    constantEnv <- use globalConstant
    functionEnv <- use env
    let typ = FunctionType returnType (fmap fst args) False
        ptrTyp = ASTType.PointerType typ (AddrSpace 0)
        ref = ASTCons.GlobalReference ptrTyp (Name $ toShortByteString name)
        functionEnv' = M.insert name (ConstantOperand ref) functionEnv
    env .= functionEnv'
    function (Name name) (fmap (Data.Bifunctor.second ParameterName) args) returnType $ \oprs -> do
        let newEnv = F.foldr (\(k,v) env' -> M.insert k v env') functionEnv' (Prelude.zip (fmap snd args) oprs)
        opr <- evalStateT (compileIRExpr body) (Env constantEnv newEnv)
        ret opr

compileIRStmts :: (MonadModuleBuilder m, MonadFix m) => [IRStmt (StateT Env (IRBuilderT (StateT Env m)))]-> StateT Env m Operand
compileIRStmts [] = error "at least one statement is required"
compileIRStmts [s] = compileIRStmt s
compileIRStmts (s:ss) = do
    compileIRStmt s
    compileIRStmts ss

compileToLLVM :: [IRStmt (StateT Env (IRBuilderT (StateT Env (ModuleBuilderT Identity))))] -> Data.Text.Lazy.Text
compileToLLVM ast =
    ppllvm $ buildModule "main" $ do
        evalStateT (compileIRStmts ast) (Env M.empty M.empty)
