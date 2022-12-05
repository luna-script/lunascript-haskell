{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

module IR (IRStmt (..), IRExpr (..), IRBlockStmt (..), ToIR (toIR), convertStmtsToIRStmts, execConvertStmtsToIRStmts, ConvertEnv, PolymorphicFunType, HasPolymorphicFun(polymorphicFun), HasEnv(env), HasGlobalConstant(globalConstant)) where

import           AST
import           Control.Lens
import           Control.Monad.State        (StateT (runStateT))
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

type PolymorphicFunType = Map P.ShortByteString (Map P.ShortByteString ([AST.Type], AST.Type))

data ConvertEnv = ConvertEnv
  { _convertEnvGlobalConstant :: M.Map P.ShortByteString AST.Type,
    _convertEnvEnv            :: M.Map P.ShortByteString AST.Type,
    _convertEnvUnusedNum      :: Int,
    _convertEnvGenerateList   :: [Stmt SimpleTyped],
    _convertEnvPolymorphicFun :: PolymorphicFunType
  }

makeFields ''ConvertEnv

data IRStmt m
  = TopLevelConst P.ShortByteString AST.Type (IRExpr m)
  | TopLevelFunDef P.ShortByteString [(AST.Type, P.ShortByteString)] AST.Type (IRExpr m)

data IRExpr m where
  IRInt :: Integer -> IRExpr m
  IRBool :: Bool -> IRExpr m
  IRUnit :: IRExpr m
  IRVector :: AST.Type -> [IRExpr m] -> IRExpr m
  IROp :: (MonadIRBuilder m) => (Operand -> Operand -> m Operand) -> IRExpr m -> IRExpr m -> IRExpr m
  IRIf :: IRExpr m -> IRExpr m -> IRExpr m -> IRExpr m
  IRVar :: P.ShortByteString -> IRExpr m
  IRFunApp :: IRExpr m -> [IRExpr m] -> IRExpr m
  IRBlock :: [IRBlockStmt m] -> IRExpr m -> IRExpr m

data IRBlockStmt m
  = IRBExprStmt (IRExpr m)
  | IRBLet P.ShortByteString (IRExpr m)

newLambdaName :: StateT ConvertEnv Identity Text
newLambdaName = do
  n <- use unusedNum
  unusedNum .= n + 1
  pure $ "$$lambda_" <> toTextStrict (show n)

class ToIR t where
  type IR t :: (* -> *) -> *
  toIR :: (MonadIRBuilder m) => t -> StateT ConvertEnv Identity (IR t m)

instance ToIR (Expr SimpleTyped) where
  type IR (Expr SimpleTyped) = IRExpr
  toIR (EInt n) = pure $ IRInt n
  toIR (EBool b) = pure $ IRBool b
  toIR EUnit = pure IRUnit
  toIR (EVector (SimpleTypedVec t xs)) = IRVector (toLLVMType t) <$> mapM toIR xs
  toIR (BinOp op lh rh) = do
    lh' <- toIR lh
    rh' <- toIR rh
    case op of
      "+"  -> pure $ IROp add lh' rh'
      "-"  -> pure $ IROp sub lh' rh'
      "*"  -> pure $ IROp mul lh' rh'
      "/"  -> pure $ IROp sdiv lh' rh'
      "==" -> pure $ IROp (icmp IP.EQ) lh' rh'
      "<"  -> pure $ IROp (icmp IP.SLT) lh' rh'
      _    -> error "unimplemented"
  toIR (EIf cond thenExpr elseExpr) = do
    cond' <- toIR cond
    thenExpr' <- toIR thenExpr
    elseExpr' <- toIR elseExpr
    pure $ IRIf cond' thenExpr' elseExpr'
  toIR (FunApp e1 e2) = do
    let (func, oprs) = separate (FunApp e1 e2)
    func' <- toIR func
    oprs' <- mapM toIR oprs
    pure $ IRFunApp func' oprs'
    where
      separate (FunApp e1 e2) =
        let (fun, args) = separate e1
         in (fun, args ++ [e2])
      separate e = (e, [])
  toIR (Var (SimpleTypedVar t name)) = do
    pFun <- use polymorphicFun
    let sName = toShortByteString name
    case M.lookup sName pFun of
      Nothing -> pure $ IRVar sName
      Just fn -> do
        let stringT = toShortByteString $ show t
        let spName = sName <> "::" <> stringT
        case M.lookup stringT fn of
          Just _ -> pure $ IRVar spName
          Nothing -> do
            let llvmT = toLLVMType t
            env' <- use env
            env .= M.insert spName llvmT env'
            let (args, resultT) = separateFunType t
            let fn' = M.insert spName (fmap toLLVMType args, toLLVMType resultT) fn
            polymorphicFun .= M.insert sName fn' pFun
            pure $ IRVar spName
  toIR e@(Fun _ _) = do
    genList <- use generateList
    name <- newLambdaName
    let t = typeOf e
    let fun = TopLevelLet (SimpleTypedVar t name) e
    generateList .= fun : genList
    pure $ IRVar $ toShortByteString name
  toIR (EBlock xs x) = do
    xs' <- mapM convertBlockStmtToIRBlockStmt xs
    x' <- toIR x
    pure $ IRBlock xs' x'
    where
      convertBlockStmtToIRBlockStmt :: (MonadIRBuilder m) => BlockStmt SimpleTyped -> StateT ConvertEnv Identity (IRBlockStmt m)
      convertBlockStmtToIRBlockStmt (BLet (SimpleTypedVar _ name) e) = do
        e' <- toIR e
        pure $ IRBLet (toShortByteString name) e'
      convertBlockStmtToIRBlockStmt (BExprStmt e) = do
        e' <- toIR e
        pure $ IRBExprStmt e'

instance ToIR (Stmt SimpleTyped) where
  type IR (Stmt SimpleTyped) = IRStmt
  toIR (TopLevelLet (SimpleTypedVar t var) (Fun (SimpleTypedVar _ name) e)) = do
    env' <- use env
    env .= M.insert (toShortByteString var) (toLLVMType t) env'
    let (args, body) = separate e
    e' <- toIR body
    let (argsType, resultType) = separateFunType t
    pure $ TopLevelFunDef (toShortByteString var) (Prelude.zip (fmap toLLVMType argsType) (toShortByteString <$> name : args)) (toLLVMType resultType) e'
    where
      separate :: Expr SimpleTyped -> ([Text], Expr SimpleTyped)
      separate (Fun (SimpleTypedVar t name) e) =
        let (args, body) = separate e
         in (name : args, body)
      separate e = ([], e)
  toIR (TopLevelLet (SimpleTypedVar t var) e) = do
    globalConstant' <- use globalConstant
    let name = toShortByteString var
        llvmtype = toLLVMType t
    globalConstant .= M.insert name llvmtype globalConstant'
    e' <- toIR e
    pure $ TopLevelConst name llvmtype e'

convertStmtsToIRStmts :: (MonadIRBuilder m) => [Stmt SimpleTyped] -> StateT ConvertEnv Identity [IRStmt m]
convertStmtsToIRStmts stmts = do
  stmt <- mapM toIR stmts
  genList <- use generateList
  newStmt <- mapM toIR genList
  pure $ stmt ++ newStmt

execConvertStmtsToIRStmts :: (MonadIRBuilder m) => [Stmt SimpleTyped] -> ([IRStmt m], ConvertEnv)
execConvertStmtsToIRStmts stmts = runIdentity $ runStateT (convertStmtsToIRStmts stmts) (ConvertEnv M.empty initialEnv 0 [] initialPolymorphicFun)

initialEnv :: Map P.ShortByteString Type
initialEnv = M.fromList [("print_int", toLLVMType $ TFun' TInt' TUnit')]

initialPolymorphicFun :: PolymorphicFunType
initialPolymorphicFun = M.fromList [("get", M.empty), ("foldl", M.empty), ("length", M.empty)]
