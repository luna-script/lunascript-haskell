{-# LANGUAGE TypeFamilies #-}

module Type where

import           Data.IORef         (IORef)
import           GHC.IORef          (readIORef)
import           LLVM.AST
import           LLVM.AST.AddrSpace (AddrSpace (..))
import           LLVM.AST.Type
import qualified LLVM.AST.Type      as ASTType

data Typ
  = TInt
  | TBool
  | TUnit
  | TFun Typ Typ
  | TVector Typ
  | TVar Integer (IORef (Maybe Typ))
  | TThunk Typ
  | QVar Integer

data Typ'
  = TInt'
  | TBool'
  | TUnit'
  | TFun' Typ' Typ'
  | TVector' Typ'
  | TThunk' Typ'
  | QVar' Integer
  deriving (Show, Eq)

showTyp :: Typ -> IO String
showTyp TInt = pure "Int"
showTyp TBool = pure "Bool"
showTyp TUnit = pure "Unit"
showTyp (TVector t) = do
  t' <- showTyp t
  pure $ "Vector (" ++ t' ++ ")"
showTyp (TFun t1 t2) = do
  t1' <- showTyp t1
  t2' <- showTyp t2
  pure $ t1' ++ "->" ++ t2'
showTyp (TVar n r) = do
  t <- readIORef r
  case t of
    Just typ -> showTyp typ
    Nothing  -> pure $ show n
showTyp (TThunk t) = do
  showt <- showTyp t
  pure $ "TThunk (" ++ showt ++ ")"
showTyp (QVar n) = pure $ "a" ++ show n

class ToTyp' t where
  type ToTyp'Result t
  toTyp' :: t -> ToTyp'Result t

instance ToTyp' Typ where
  type ToTyp'Result Typ = IO Typ'
  toTyp' TInt = pure TInt'
  toTyp' TBool = pure TBool'
  toTyp' TUnit = pure TUnit'
  toTyp' (QVar n) = pure $ QVar' n
  toTyp' (TFun t1 t2) = do
    t1' <- toTyp' t1
    t2' <- toTyp' t2
    pure $ TFun' t1' t2'
  toTyp' (TVector t) = TVector' <$> toTyp' t
  toTyp' (TThunk t) = do
    t' <- toTyp' t
    pure $ TThunk' t'
  toTyp' (TVar _ r) = do
    t <- readIORef r
    case t of
      Nothing -> error "unspecialized tvar"
      Just t' -> toTyp' t'

convertTypPrimeTollvmType :: Typ' -> ASTType.Type
convertTypPrimeTollvmType TInt' = ASTType.i32
convertTypPrimeTollvmType TBool' = ASTType.i1
convertTypPrimeTollvmType TUnit' = unitType
convertTypPrimeTollvmType (QVar' n) = error $ "generic TVar " ++ show n
convertTypPrimeTollvmType (TVector' t) = vectorType
convertTypPrimeTollvmType (TThunk' t) = ASTType.PointerType (ASTType.FunctionType (convertTypPrimeTollvmType t) [] False) (AddrSpace 0)
convertTypPrimeTollvmType (TFun' t1 t2) =
  let separateArgsAndResultType :: Typ' -> ([Typ'], Typ')
      separateArgsAndResultType (TFun' t1_ t2_) =
        let (args_, result_) = separateArgsAndResultType t2_
         in (t1_ : args_, result_)
      separateArgsAndResultType t = ([], t)
      (args, result) = separateArgsAndResultType t2
   in ASTType.PointerType (ASTType.FunctionType (convertTypPrimeTollvmType result) (fmap convertTypPrimeTollvmType $ t1 : args) False) (AddrSpace 0)

separateFunType :: Typ' -> ([Typ'], Typ')
separateFunType (TFun' arg t) =
  let (args, result) = separateFunType t
   in (arg : args, result)
separateFunType t = ([], t)

unitType :: Type
unitType = StructureType False []

vectorType :: Type
vectorType = ptr rawVectorType

rawVectorType :: Type
rawVectorType = StructureType False [i32]

vector1Type :: Type -> Type
vector1Type = ptr . rawVector1Type

rawVector1Type :: Type -> Type
rawVector1Type t = StructureType False [i32, ArrayType 32 t]

vector2Type :: Type -> Type
vector2Type = ptr . rawVector2Type

rawVector2Type :: Type -> Type
rawVector2Type t = StructureType False [i32, ArrayType 32 $ ArrayType 32 t]

foldlType :: Typ
foldlType = TFun (TFun (QVar 1) (TFun (QVar 0) $ QVar 1)) $ TFun (QVar 1) $ TFun (TVector $ QVar 0) $ QVar 1

foldlLlvmType :: Type
foldlLlvmType = convertTypPrimeTollvmType $ TFun' (TFun' TInt' $ TFun' TInt' TInt') $ TFun' TInt' $ TFun' (TVector' TInt') TInt'
