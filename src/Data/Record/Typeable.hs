-- Copyright (c) 2011, Mark Wright.  All rights reserved.

{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
    ScopedTypeVariables, StandaloneDeriving, TypeOperators, 
    UndecidableInstances #-}

-- | Typeable instances for Data.Record
module Data.Record.Typeable where

import Data.Kind
import Data.TypeFun
import Data.Record
import Data.Typeable

-- | KindStar Typeable instance.
deriving instance Typeable KindStar

-- | (Id KindStar) Typeable instance.
deriving instance Typeable KindStar =>
  Typeable1 Id

-- | Empty record scheme Typeable instance.
instance Typeable style => 
  Typeable (X style) where
    typeOf (_::X style) = mkTyConApp (mkTyCon "Data.Record.X") [ typeOf (undefined::style) ]

-- | Non-empty record schemes Typeable instances.
instance (Typeable (rec style), Typeable name, Typeable (App style sort)) => 
  Typeable ((rec :& name ::: sort) style) where
    typeOf (rec :& field) = mkTyConApp (mkTyCon "Data.Record.:&") [ typeOf rec, typeOf field ]

-- | Record fields Typeable instances.
instance (Typeable name, Typeable (App style sort)) =>
  Typeable ((name ::: sort) style) where
    typeOf (name := val) = mkTyConApp (mkTyCon "Data.Record.:=") [ typeOf name, typeOf val ]
