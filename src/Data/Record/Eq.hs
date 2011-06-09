{-# LANGUAGE FlexibleContexts, FlexibleInstances, StandaloneDeriving, TypeOperators #-}

module Data.Record.Eq where

import Data.Kind
import Data.TypeFun
import Data.Record
import Data.Typeable

-- | Eq instance for KindStar
deriving instance Eq KindStar

-- | Eq instance for Id KindStar
deriving instance Eq KindStar =>
  Eq (Id KindStar)

-- | Eq instance for X style
deriving instance Eq style =>
  Eq (X style)

-- | Eq instance for :&
deriving instance (Eq (rec style), Eq name, Eq (App style sort)) => 
  Eq ((rec :& name ::: sort) style)

-- | Eq instance for name
deriving instance (Eq name, Eq (App style sort)) =>
  Eq ((name ::: sort) style)
