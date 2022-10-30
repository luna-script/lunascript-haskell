{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
module IR where

import           AST
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State        (StateT (runStateT))
import           Data.Functor.Identity      (Identity (..))
import           Data.Map                   as M
import           Data.String.Transform
import           Data.Text
import           LLVM.AST                   (Operand)
import qualified LLVM.AST                   as AST
import           LLVM.AST.IntegerPredicate  as IP
import           LLVM.AST.Type
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Monad
import qualified LLVM.Prelude               as P
import           Type
import           TypInf                     hiding (TEnv)

data ConvertEnv = ConvertEnv {
        _convertEnvGlobalConstant :: M.Map P.ShortByteString AST.Type,
        _convertEnvEnv            :: M.Map P.ShortByteString AST.Type
    }

makeFields ''ConvertEnv

data IRStmt m = TopLevelConst P.ShortByteString AST.Type (IRExpr m)
    | TopLevelThunkDef P.ShortByteString AST.Type (IRExpr m)
    | TopLevelFunDef P.ShortByteString [(AST.Type, P.ShortByteString)] AST.Type (IRExpr m)

data IRExpr m where
  IRInt :: Integer -> IRExpr m
  IRBool :: Bool -> IRExpr m
  IRUnit :: IRExpr m
  IROp :: (MonadIRBuilder m) => (Operand -> Operand -> m Operand) -> IRExpr m -> IRExpr m -> IRExpr m
  IRIf :: IRExpr m -> IRExpr m -> IRExpr m -> IRExpr m
  IRVar :: P.ShortByteString -> IRExpr m
  IRFunApp :: IRExpr m -> [IRExpr m] -> IRExpr m
  IRExecThunk :: IRExpr m -> IRExpr m
  IRBlock :: [IRBlockStmt m] -> IRExpr m -> IRExpr m

data IRBlockStmt m = IRBExprStmt (IRExpr m)
    | IRBLet P.ShortByteString (IRExpr m)

convertExprToIRExpr :: (MonadIRBuilder m) => Expr SimpleTyped -> StateT ConvertEnv Identity (IRExpr m)
convertExprToIRExpr (EInt n) = pure $ IRInt n
convertExprToIRExpr (EBool b) = pure $ IRBool b
convertExprToIRExpr EUnit = pure IRUnit
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
convertExprToIRExpr (EBlock xs x) = do
    xs' <- mapM convertBlockStmtToIRBlockStmt xs
    x' <- convertExprToIRExpr x
    pure $ IRBlock xs' x'
    where
        convertBlockStmtToIRBlockStmt :: (MonadIRBuilder m) => BlockStmt SimpleTyped -> StateT ConvertEnv Identity (IRBlockStmt m)
        convertBlockStmtToIRBlockStmt (BLet (SimpleTypedVar _ name) e) = do
            e' <- convertExprToIRExpr e
            pure $ IRBLet (toShortByteString name) e'
        convertBlockStmtToIRBlockStmt (BExprStmt e) = do
            e' <- convertExprToIRExpr e
            pure $ IRBExprStmt e'

convertStmtToIRStmt :: (MonadIRBuilder m) => Stmt SimpleTyped -> StateT ConvertEnv Identity (IRStmt m)
convertStmtToIRStmt (TopLevelLet (SimpleTypedVar t var) (EThunk e)) = do
    env' <- use env
    let name = toShortByteString var
        llvmType = convertTypPrimeTollvmType t
        t' = case t of
            TThunk' t_ -> t_
            _          -> error "Internal Error"
    env .= M.insert name llvmType env'
    e' <- convertExprToIRExpr e
    pure $ TopLevelThunkDef name (convertTypPrimeTollvmType t') e'
convertStmtToIRStmt (TopLevelLet (SimpleTypedVar t var) (Fun (SimpleTypedVar _ name) e)) = do
    env' <- use env
    env .= M.insert (toShortByteString var) (convertTypPrimeTollvmType t) env'
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
    globalConstant' <- use globalConstant
    let name = toShortByteString var
        llvmtype = convertTypPrimeTollvmType t
    globalConstant .= M.insert name llvmtype globalConstant'
    e' <- convertExprToIRExpr e
    pure $ TopLevelConst name llvmtype e'

convertStmtsToIRStmts :: (MonadIRBuilder m) => [Stmt SimpleTyped] -> StateT ConvertEnv Identity [IRStmt m]
convertStmtsToIRStmts = mapM convertStmtToIRStmt

execConvertStmtsToIRStmts :: (MonadIRBuilder m) => [Stmt SimpleTyped] -> ([IRStmt m], ConvertEnv)
execConvertStmtsToIRStmts stmts = runIdentity $ runStateT (convertStmtsToIRStmts stmts) (ConvertEnv M.empty initialEnv)

initialEnv :: Map P.ShortByteString Type
initialEnv = M.fromList [("print_int", convertTypPrimeTollvmType $ TFun' TInt' TUnit')]

unitType :: Type
unitType = StructureType False []
