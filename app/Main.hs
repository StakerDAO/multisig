module Main
  ( main
  ) where

import qualified Data.Map as Map
import Fmt (pretty)
import qualified Lorentz as L
import qualified Lorentz.Contracts.Multisig as Msig
import qualified Morley.Client as MC
import qualified Morley.Client.RPC as MC
import qualified Morley.Micheline as M
import qualified Morley.Michelson.Untyped as MU
import qualified Options.Applicative as Opt

import Data.Set (fromList)
import Morley.Tezos.Address (Address(..))
import Morley.Tezos.Crypto
  (SecretKey, Signature, formatPublicKey, formatSignature, hashKey, sign, toPublic)

import Parser

runWithClient
  :: MC.MorleyClientConfig
  -> MC.MorleyClientM a -> IO a
runWithClient conf action = do
  env <- MC.mkMorleyClientEnv conf
  MC.runMorleyClientM env action

runNoClient
  :: MC.MorleyClientConfig
  -> [SecretKey]
  -> MC.MorleyOnlyRpcM a -> IO a
runNoClient MC.MorleyClientConfig{..} skeys action = do
  case mccEndpointUrl of
    Just ep -> do
      env <- MC.mkMorleyOnlyRpcEnv skeys ep mccVerbosity
      MC.runMorleyOnlyRpcM env action
    Nothing ->
      error "Endpoint URL must be specified"

data AppError =
  UnknownEntrypoint MU.EpName
  | GetNonceError M.FromExpressionError
  | UnknownSkAlias Text
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
        runNoClient siClientConfig [sk] $
        signDo (liftIO . sign sk) (toPublic sk)
          siNonce siCommand siMultisigContract
      AliasSk al ->
        runWithClient siClientConfig $ do
        pk <- MC.getPublicKey al
        signDo (MC.signBytes al Nothing) pk
          siNonce siCommand siMultisigContract
  Submit (SubmitCliCommand {..}) -> do
    opHash <- case suFeePayer of
      Sk feePayerSk -> runNoClient suClientConfig [feePayerSk] $
        let feePayerAddr = KeyAddress $ hashKey $ toPublic feePayerSk
        in doCall
          (MC.transfer feePayerAddr)
          suMultisigContract suCommand suNonce suSignatures
      AliasSk al -> runWithClient suClientConfig $ do
        addr <- maybe (throwM $ UnknownSkAlias $ show al) pure =<<
                  MC.resolveAddressMaybe al
        doCall
          (MC.transfer addr)
          suMultisigContract suCommand suNonce suSignatures
    putTextLn $ "Submitted operation " <> pretty opHash

doCall
  :: MC.HasTezosRpc m
  => ( L.Address
       -> L.Mutez
       -> MU.EpName
       -> L.Value (L.ToT Msig.Parameter)
       -> Maybe L.Mutez
       -> m MC.OperationHash )
  -> L.Address
  -> MsigCommand
  -> Maybe Natural
  -> Msig.Signatures
  -> m MC.OperationHash
doCall transferF msigAddr cmd mNonce signatures = do
  order <- mkOrder cmd
  nonce <- maybe (nextNonce msigAddr) pure mNonce
  let val = L.toVal $ Msig.Parameter {..}
  transferF msigAddr
    L.zeroMutez (MU.UnsafeEpName "default") val Nothing
