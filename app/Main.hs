module Main
  ( main
  ) where

import Universum

import qualified Data.Map as Map
import qualified Lorentz as L
import qualified Lorentz.Contracts.Multisig as Msig
import qualified Options.Applicative as Opt
import qualified Morley.Client as MC
import qualified Morley.Client.RPC as MC
import Tezos.Crypto (sign, formatSignature, formatPublicKey, Signature, toPublic)
import qualified Michelson.Untyped as MU
import qualified Morley.Micheline as M

import Data.Set (fromList)

import Parser

runWithClient
  :: MC.MorleyClientConfig
  -> MC.MorleyClientM a -> IO a
runWithClient conf action = do
  env <- MC.mkMorleyClientEnv conf
  MC.runMorleyClientM env action

runNoClient
  :: MC.MorleyClientConfig
  -> MC.MorleyNoClientM a -> IO a
runNoClient conf action = do
  env <- MC.mkMorleyNoClientEnv conf
  MC.runMorleyNoClientM env action

data AppError =
  UnknownEntrypoint MU.EpName
  | GetNonceError M.FromExpressionError
  | UnknownSkAlias
  deriving stock Show
  deriving anyclass Exception

mkOrder
  :: MC.HasTezosRpc m
  => MsigCommand -> m Msig.Order
mkOrder (RotateKeys keyHashes) = pure $
  Msig.mkRotateKeysOrder (fromList keyHashes)
mkOrder (Call call@Msig.CallArgs {..}) = do
  origCodeExpr <- MC.osCode <$> MC.getContractScript caContract
  up <-
    either throwM (pure . MU.contractParameter) $
    M.fromExpression @MU.Contract origCodeExpr
  epType <-
    maybe (throwM (UnknownEntrypoint caEntrypoint)) pure $
      Map.lookup caEntrypoint (MU.mkEntrypointsMap up)
  either throwM pure $ Msig.mkCallOrder call epType

nextNonce
  :: MC.HasTezosRpc m
  => L.Address
  -> m Natural
nextNonce msigAddr = do
  storageExpr <- MC.getContractStorage msigAddr
  Msig.Storage {..} <-
    either (throwM . GetNonceError) (pure . L.fromVal)
      (M.fromExpression storageExpr)
  pure $ currentNonce + 1

signDo
  ::
    ( MonadIO m
    , MC.HasTezosRpc m
    )
  => (ByteString -> m Signature)
  -> L.PublicKey
  -> Maybe Natural
  -> MsigCommand
  -> L.Address
  -> m ()
signDo signF pk mNonce cmd msigAddr = do
  order <- mkOrder cmd
  nonce <- maybe (nextNonce msigAddr) pure mNonce
  let valueToSign =
        Msig.ValueToSign
        { vtsMultisigAddress = msigAddr
        , vtsNonce = nonce
        , vtsOrder = order
        }
  signature <- signF (L.unPacked $ L.lPackValue valueToSign)
  putTextLn $ formatPublicKey pk <> ":" <> formatSignature signature

main :: IO ()
main = Opt.execParser cmdParser >>= \case
  PrintContract -> putStr $ L.printLorentzContract False $
    L.defaultContract Msig.multisigContract
  PrintStorage keyHashes -> putStr $ L.printLorentzValue False $
    Msig.Storage (fromList keyHashes) 0
  Sign (SignCliCommand {..}) ->
    case siSk of
      Sk sk ->
        runNoClient siClientConfig $
        signDo (sign sk) (toPublic sk)
          siNonce siCommand siMultisigContract
      AliasSk al ->
        runWithClient siClientConfig $ do
        pk <- MC.getPublicKey al
        signDo (MC.signBytes al) pk
          siNonce siCommand siMultisigContract
  Submit (SubmitCliCommand {..}) -> do
    opHash <- case suFeePayer of
      Sk feePayerSk -> runNoClient suClientConfig $
        doCall
          (MC.transferNoClient feePayerSk)
          suMultisigContract suCommand suNonce suSignatures
      AliasSk al -> runWithClient suClientConfig $ do
        addr <- maybe (throwM UnknownSkAlias) pure =<<
                  MC.resolveAddressMaybe al
        doCall
          (MC.transfer addr)
          suMultisigContract suCommand suNonce suSignatures
    putTextLn $ "Submitted operation " <> opHash

doCall
  :: MC.HasTezosRpc m
  => ( L.Address
       -> L.Mutez
       -> MU.EpName
       -> L.Value (L.ToT Msig.Parameter)
       -> Maybe L.Mutez
       -> m Text )
  -> L.Address
  -> MsigCommand
  -> Maybe Natural
  -> Msig.Signatures
  -> m Text
doCall transferF msigAddr cmd mNonce signatures = do
  order <- mkOrder cmd
  nonce <- maybe (nextNonce msigAddr) pure mNonce
  transferF msigAddr
    L.zeroMutez
    (MU.EpNameUnsafe "Default")
    (L.toVal $ Msig.Parameter {..}) Nothing
