module Options
  ( pkHashArg
  , sigPairOpt
  , entrypointOpt
  , nonceOpt
  , paramOption
  ) where

import Universum

import qualified Lorentz as L
import qualified Michelson.Parser as M
import qualified Michelson.Untyped as M
import qualified Options.Applicative as Opt

import Fmt (pretty)
import Tezos.Crypto (KeyHash, PublicKey, parseKeyHash, parsePublicKey, parseSignature)

keyHashReader :: Opt.ReadM KeyHash
keyHashReader = Opt.eitherReader $ \addr ->
   either
        (Left . mappend "Failed to parse key hash: " . pretty)
        Right $
        parseKeyHash $ toText addr

pkHashArg :: Opt.Parser KeyHash
pkHashArg = Opt.argument keyHashReader $ Opt.metavar "PK_HASH"

sigPairOpt :: Opt.Parser (PublicKey, L.TSignature ByteString)
sigPairOpt = Opt.option sigPairReader $ mconcat
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

entrypointOpt :: Opt.Parser M.EpName
entrypointOpt = Opt.option entrypointReader $ mconcat
  [ Opt.metavar "ENTRYPOINT_NAME"
  , Opt.long "entrypoint"
  ]
  where
    entrypointReader = Opt.eitherReader $ \epStr ->
      either (Left . mappend "Failed to parse entrypoint name: " . pretty) Right $
        M.buildEpName $ toText epStr

nonceOpt :: Opt.Parser Natural
nonceOpt = Opt.option Opt.auto $ mconcat
  [ Opt.metavar "NONCE"
  , Opt.long "nonce"
  ]

paramOption :: Opt.Parser M.Value
paramOption = Opt.option valueReader $ mconcat
  [ Opt.metavar "MICHELSON_VALUE"
  , Opt.long "param"
  ]
  where
    valueReader :: Opt.ReadM M.Value
    valueReader = Opt.eitherReader $ \paramStr ->
      either (Left . mappend "Failed to parse Michelson value: " . pretty) Right $
        M.parseExpandValue $ toText paramStr
