module Type where
import           Data.IORef         (IORef)
import           GHC.IORef          (readIORef)
import           LLVM.AST
import           LLVM.AST.AddrSpace (AddrSpace (..))
import           LLVM.AST.Type
import qualified LLVM.AST.Type      as ASTType

data Typ = TInt
    | TBool
    | TUnit
    | TFun Typ Typ
    | TVector Typ
    | TVar Integer (IORef (Maybe Typ))
    | TThunk Typ


data Typ' = TInt'
    | TBool'
    | TUnit'
    | TFun' Typ' Typ'
    | TVector' Typ'
    | TThunk' Typ'
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

convertTypToTyp' :: Typ -> IO Typ'
convertTypToTyp' TInt = pure TInt'
convertTypToTyp' TBool = pure TBool'
convertTypToTyp' TUnit = pure TUnit'
convertTypToTyp' (TFun t1 t2) = do
    t1' <- convertTypToTyp' t1
    t2' <- convertTypToTyp' t2
    pure $ TFun' t1' t2'
convertTypToTyp' (TVector t) = TVector' <$> convertTypToTyp' t
convertTypToTyp' (TThunk t) = do
    t' <- convertTypToTyp' t
    pure $ TThunk' t'
convertTypToTyp' (TVar _ r) = do
    t <- readIORef r
    case t of
        Nothing -> error "unspecialized tvar"
        Just t' -> convertTypToTyp' t'

convertTypPrimeTollvmType :: Typ' -> ASTType.Type
convertTypPrimeTollvmType TInt'         = ASTType.i32
convertTypPrimeTollvmType TBool'        = ASTType.i1
convertTypPrimeTollvmType TUnit'        = unitType
convertTypPrimeTollvmType (TVector' t)  = vectorType (convertTypPrimeTollvmType t)
convertTypPrimeTollvmType (TThunk' t)   = ASTType.PointerType (ASTType.FunctionType (convertTypPrimeTollvmType t) [] False) (AddrSpace 0)
convertTypPrimeTollvmType (TFun' t1 t2) = let
    separateArgsAndResultType :: Typ' -> ([Typ'], Typ')
    separateArgsAndResultType (TFun' t1_ t2_) = let
        (args_, result_) = separateArgsAndResultType t2_
        in (t1_:args_, result_)
    separateArgsAndResultType t = ([], t)
    (args, result) = separateArgsAndResultType t2
    in ASTType.PointerType (ASTType.FunctionType (convertTypPrimeTollvmType result) (fmap convertTypPrimeTollvmType $ t1:args) False) (AddrSpace 0)

unitType :: Type
unitType = StructureType False []

vectorType :: Type -> Type
vectorType t = ptr (StructureType False [i32, ptr $ ArrayType 32 t, ptr $ ArrayType 32 (ArrayType 32 t)])
