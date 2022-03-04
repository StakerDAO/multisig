module Options
  ( pkHashArg
  , sigPairOption
  , entrypointOption
  , nonceOption
  , paramOption
  , contractOption
  , multisigContractOption
  ) where

import qualified Lorentz as L
import qualified Morley.Michelson.Untyped as M
import qualified Options.Applicative as Opt

import Fmt (pretty)
import qualified Morley.CLI as M
import Morley.Tezos.Address (Address)
import Morley.Tezos.Crypto (KeyHash, PublicKey, parsePublicKey, parseSignature)
import qualified Morley.Util.CLI as M

pkHashArg :: Opt.Parser KeyHash
pkHashArg = Opt.argument M.getReader $ Opt.metavar (M.getMetavar @KeyHash)

sigPairOption :: Opt.Parser (PublicKey, L.TSignature ByteString)
sigPairOption = Opt.option sigPairReader $ mconcat
  [ Opt.metavar "PK:SIG"
  , Opt.long "signed"
  ]
  where
    sigPairReader :: Opt.ReadM (PublicKey, L.TSignature ByteString)
    sigPairReader = Opt.eitherReader $ \sigPairStr ->
      either (Left . mappend "Failed to parse signature pair: ") Right $
      let (l, r) = break (== ':') sigPairStr
      in do
        sigStr <- maybe (Left "':' must be present in signature pair") Right $
          tail <$> nonEmpty r
        either (Left . pretty) Right $ do
          pk <- parsePublicKey $ toText l
          sig <- L.TSignature <$> (parseSignature $ toText sigStr)
          pure $ (pk, sig)

entrypointOption :: Opt.Parser M.EpName
entrypointOption = M.entrypointOption (#name "entrypoint")
  (#help "entrypoint to call on the target contract")

nonceOption :: Opt.Parser Natural
nonceOption = Opt.option Opt.auto $ mconcat
  [ Opt.metavar "NONCE"
  , Opt.long "nonce"
  ]

paramOption :: Opt.Parser M.Value
paramOption = M.valueOption Nothing (#name "param")
  (#help "parameter of the call to target contract")

-- | Parse address of the contract to call.
contractOption :: Opt.Parser Address
contractOption = M.addressOption Nothing (#name "target-contract")
  (#help "address of target contract")

multisigContractOption :: Opt.Parser Address
multisigContractOption = M.addressOption Nothing (#name "multisig-contract")
  (#help "address of multisig contract")
