module Type where

import Data.IORef (IORef)

data Typ
  = TInt
  | TBool
  | TUnit
  | TPair Typ Typ
  | TFun Typ Typ
  | TRef Typ
  | TVector Typ
  | TVar Integer (IORef (Maybe Typ))
  | QVar Integer

data Typ'
  = TInt'
  | TBool'
  | TUnit'
  | TPair' Typ' Typ'
  | TFun' Typ' Typ'
  | TRef' Typ'
  | TVector' Typ'
  | QVar' Integer
