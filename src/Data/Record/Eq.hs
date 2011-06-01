{-# LANGUAGE FlexibleContexts, FlexibleInstances, StandaloneDeriving, TypeOperators #-}

module Data.Record.Eq where

import Data.Kind
import Data.TypeFun
import Data.Record
import Data.Typeable

deriving instance Eq KindStar

deriving instance Eq KindStar =>
  Eq (Id KindStar)

deriving instance Eq style =>
  Eq (X style)

deriving instance (Eq (rec style), Eq name, Eq (App style sort)) => 
  Eq ((rec :& name ::: sort) style)

deriving instance (Eq name, Eq (App style sort)) =>
  Eq ((name ::: sort) style)
