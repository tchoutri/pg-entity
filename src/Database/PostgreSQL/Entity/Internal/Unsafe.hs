{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
  Module      : Database.PostgreSQL.Entity.Internal.Unsafe
  Copyright   : © Clément Delafargue, 2018
                  Théophile Choutri, 2021
                  Koz Ross, 2021
  License     : MIT
  Maintainer  : theophile@choutri.eu
  Stability   : Experimental

  Contains the internals of several key types.

  = Note

  By using these directly, you run the risk of violating internal invariants,
  or making representational changes in incompatible ways. This API is not
  stable, and is not subject to the PVP. Use at your own risk

  If at all possible, instead use the API provided by
  'Database.PostgreSQL.Entity.Types'.
-}
module Database.PostgreSQL.Entity.Internal.Unsafe
  ( Field (..)
  )
where

import Data.Kind
import Data.String
import Data.Text (Text)
import GHC.TypeLits

{-| A wrapper for table fields.

 @since 0.0.1.0
-}
data Field
  = Field Text (Maybe Text)
  deriving stock (Eq, Show)

-- | Using the Overloaded String syntax for Field names is forbidden.
instance ForbiddenIsString => IsString Field where
  fromString = error "You cannot pass a field as a string. Please use the `field` quasi-quoter instead."

type family ForbiddenIsString :: Constraint where
  ForbiddenIsString =
    TypeError
      ( 'Text "🚫 You cannot pass a Field name as a string."
          ':$$: 'Text "Please use the `field` quasi-quoter instead."
      )
