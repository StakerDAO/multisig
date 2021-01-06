module Lorentz.Contracts.Multisig.Storage
  ( Storage (..)
  ) where

import Lorentz

data Storage = Storage
  { teamKeys :: Set KeyHash
  , currentNonce :: Natural
  }
  deriving stock Generic
  deriving anyclass (IsoValue, HasAnnotation)
