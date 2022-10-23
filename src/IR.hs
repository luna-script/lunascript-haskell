{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module IR where

import           AST
import           Control.Monad.Reader
import           Data.Functor.Identity      (Identity (..))
import           Data.Map                   as M
import           Data.String.Transform
import           Data.Text
import           LLVM.AST                   (Operand)
import qualified LLVM.AST                   as AST
import           LLVM.AST.IntegerPredicate  as IP
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Monad
import qualified LLVM.Prelude               as P
import           Type
import           TypInf                     hiding (TEnv)

data IRStmt m = TopLevelConst P.ShortByteString AST.Type (IRExpr m)
    | TopLevelThunkDef P.ShortByteString AST.Type (IRExpr m)
    | TopLevelFunDef P.ShortByteString [(AST.Type, P.ShortByteString)] AST.Type (IRExpr m)

data IRExpr m where
  IRInt :: Integer -> IRExpr m
  IRBool :: Bool -> IRExpr m
  IROp :: (MonadIRBuilder m) => (Operand -> Operand -> m Operand) -> IRExpr m -> IRExpr m -> IRExpr m
  IRIf :: IRExpr m -> IRExpr m -> IRExpr m -> IRExpr m
  IRVar :: P.ShortByteString -> IRExpr m
  IRFunApp :: IRExpr m -> [IRExpr m] -> IRExpr m
  IRExecThunk :: IRExpr m -> IRExpr m

convertExprToIRExpr :: (MonadIRBuilder m) => Expr SimpleTyped -> Identity (IRExpr m)
convertExprToIRExpr (EInt n) = pure $ IRInt n
convertExprToIRExpr (EBool b) = pure $ IRBool b
convertExprToIRExpr (BinOp op lh rh) = do
    lh' <- convertExprToIRExpr lh
    rh' <- convertExprToIRExpr rh
    case op of
        "+"  -> pure $ IROp add lh' rh'
        "-"  -> pure $ IROp sub lh' rh'
        "*"  -> pure $ IROp mul lh' rh'
        "/"  -> pure $ IROp sdiv lh' rh'
        "==" -> pure $ IROp (icmp IP.EQ) lh' rh'
        "<"  -> pure $ IROp (icmp IP.SLT) lh' rh'
        _    -> error "unimplemented"
convertExprToIRExpr (EIf cond thenExpr elseExpr) = do
    cond' <- convertExprToIRExpr cond
    thenExpr' <- convertExprToIRExpr thenExpr
    elseExpr' <- convertExprToIRExpr elseExpr
    pure $ IRIf cond' thenExpr' elseExpr'
convertExprToIRExpr (FunApp e1 e2) = do
    let (func, oprs) = separate (FunApp e1 e2)
    func' <- convertExprToIRExpr func
    oprs' <- mapM convertExprToIRExpr oprs
    pure $ IRFunApp func' oprs'
    where
        separate (FunApp e1 e2) = let
            (fun, args) = separate e1
            in (fun, args ++ [e2])
        separate e = (e, [])
convertExprToIRExpr (Var (SimpleTypedVar _ name)) = pure $ IRVar $ toShortByteString name
convertExprToIRExpr (ExecThunk e) = IRExecThunk <$> convertExprToIRExpr e
convertExprToIRExpr (Fun _ _) = error "unimplemented"
convertExprToIRExpr (EThunk _) = error "unimplemented"

convertStmtToIRStmt :: (MonadIRBuilder m) => Stmt SimpleTyped -> Identity (IRStmt m)
convertStmtToIRStmt (TopLevelLet (SimpleTypedVar t var) (EThunk e)) = do
    e' <- convertExprToIRExpr e
    pure $ TopLevelThunkDef (toShortByteString var) (convertTypPrimeTollvmType t) e'
convertStmtToIRStmt (TopLevelLet (SimpleTypedVar t var) (Fun (SimpleTypedVar _ name) e)) = do
    let (args, body) = separate e
    e' <- convertExprToIRExpr body
    let (argsType, resultType) = separateType t
    pure $ TopLevelFunDef (toShortByteString var) (Prelude.zip (fmap convertTypPrimeTollvmType argsType) (toShortByteString <$> name:args)) (convertTypPrimeTollvmType resultType) e'
    where
        separate :: Expr SimpleTyped -> ([Text], Expr SimpleTyped)
        separate (Fun (SimpleTypedVar t name) e) = let
            (args, body) = separate e
            in (name:args, body)
        separate e = ([], e)
        separateType (TFun' arg t) = let
            (args, result) = separateType t
            in (arg:args, result)
        separateType t = ([], t)
convertStmtToIRStmt (TopLevelLet (SimpleTypedVar t var) e) = do
    e' <- convertExprToIRExpr e
    pure $ TopLevelConst (toShortByteString var) (convertTypPrimeTollvmType t) e'

convertStmtsToIRStmts :: (MonadIRBuilder m) => [Stmt SimpleTyped] -> Identity [IRStmt m]
convertStmtsToIRStmts = mapM convertStmtToIRStmt

execConvertStmtsToIRStmts :: (MonadIRBuilder m) => [Stmt SimpleTyped] -> [IRStmt m]
execConvertStmtsToIRStmts stmts = runIdentity $ convertStmtsToIRStmts stmts
