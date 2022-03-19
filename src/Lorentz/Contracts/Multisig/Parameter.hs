module Lorentz.Contracts.Multisig.Parameter
  ( Order (..)
  , Parameter (..)
  , Signatures
  , ValueToSign (..)
  , CallArgs (..)
  , mkCallOrder
  , mkRotateKeysOrder
  ) where

import qualified Morley.Michelson.TypeCheck as M
import qualified Morley.Michelson.Untyped as M
import qualified Morley.Michelson.Untyped as U

import Data.Constraint ((\\))
import Lorentz hiding (Call)
import Morley.Michelson.Typed

import Lorentz.Contracts.Multisig.Error ()

type Signatures = [(PublicKey, TSignature ByteString)]

data CallArgs = CallArgs
  { caEntrypoint :: M.EpName
  , caParam :: M.Value
  , caContract :: Address
  }

data Parameter = Parameter
  { order :: Order
  , nonce :: Natural
  , signatures :: Signatures
  } deriving stock Generic
    deriving anyclass (IsoValue, HasAnnotation)

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdNone

-- | Action that is going to be executed. May be either some
-- contract call, or key rotation.
data Order
  = Call (Lambda () Operation)
  | RotateKeys (Set KeyHash)
  deriving stock Generic
  deriving anyclass (IsoValue, HasAnnotation)

-- | Value that the participants should sign. Includes nonce to
-- prevent replay attacks and the multisig address to prevent both
-- cross-address and cross-chain replays.
data ValueToSign = ValueToSign
  { vtsMultisigAddress :: Address
  , vtsNonce :: Natural
  , vtsOrder :: Order
  } deriving stock Generic
    deriving anyclass IsoValue

mkRotateKeysOrder :: Set KeyHash -> Order
mkRotateKeysOrder = RotateKeys

data MkCallOrderError =
  WrongParameter (Maybe M.TCError)
  deriving stock Show
  deriving anyclass Exception

mkCallOrder
  :: CallArgs
  -> M.Ty
  -> Either MkCallOrderError Order
mkCallOrder CallArgs {..} epType =
  withUType epType $ \(_notes :: Notes t) -> do
    (tcValue :: Value t) <- first (WrongParameter . Just) $ M.typeCheckingWith def $
      M.typeVerifyParameter mempty caParam
    let lam = do
          let s = sing @t
          -- For tcValue
          Dict <- contractTypeAbsense s
          Dict <- bigMapAbsense s
          -- For CONTRACT
          Dict <- opAbsense s
          Dict <- nestedBigMapsAbsense s
          case checkTicketPresence s of
            TicketPresent -> Nothing
            TicketAbsent ->
              Just $
                DROP `Seq`
                PUSH (VAddress (EpAddress caContract U.DefEpName)) `Seq`
                CONTRACT starNotes caEntrypoint `Seq`
                IF_NONE (PUSH (VString [mt|invalidParamType|]) `Seq` FAILWITH)
                    Nop `Seq`
                PUSH (VMutez zeroMutez) `Seq`
                PUSH tcValue `Seq`
                TRANSFER_TOKENS
    maybe (Left $ WrongParameter Nothing)
      (\l ->
        pure $ Call $
          I l \\ niceParameterEvi @(Lambda () Operation) )
      lam
