module Parser
  ( cmdParser
  , CallArgs (..)
  , MsigCommand (..)
  , CliCommand (..)
  , SecretKeyConf (..)
  , SignCliCommand (..)
  , SubmitCliCommand (..)
  ) where

import Universum

import qualified Options.Applicative as Opt

import Options.Applicative (command, helper, info, progDesc)
import Tezos.Crypto (KeyHash, SecretKey)
import Tezos.Address (Address)
import qualified Morley.CLI as M
import Util.CLI (mkCLOptionParser)
import qualified Morley.Client as M

import Lorentz.Contracts.Multisig (Signatures, CallArgs (..))

import Options

data SecretKeyConf =
    Sk SecretKey
  | AliasSk M.AddressOrAlias

secretKeyConf
  :: String
  -> String
  -> String
  -> String
  -> Opt.Parser SecretKeyConf
secretKeyConf skName skHelp aliasName aliasHelp =
  Sk <$> sk <|>
  AliasSk <$> aliasSk
  where
    sk =
      M.secretKeyOption Nothing
        (#name skName) (#help skHelp)
    aliasSk =
      mkCLOptionParser Nothing
        (#name aliasName) (#help aliasHelp)

data MsigCommand
  = RotateKeys [KeyHash]
  | Call CallArgs

data SignCliCommand = SignCliCommand
  { siCommand :: MsigCommand
  , siMultisigContract :: Address
  , siNonce :: Maybe Natural
  , siSk :: SecretKeyConf
  , siClientConfig :: M.MorleyClientConfig
  }

data SubmitCliCommand = SubmitCliCommand
  { suCommand :: MsigCommand
  , suMultisigContract :: Address
  , suNonce :: Maybe Natural
  , suFeePayer :: SecretKeyConf
  , suSignatures :: Signatures
  , suClientConfig :: M.MorleyClientConfig
  }

data CliCommand
  = PrintContract
  | PrintStorage [KeyHash]
  | Sign SignCliCommand
  | Submit SubmitCliCommand

mkCmdPrs
  :: String
  -> String
  -> Opt.Parser a
  -> Opt.Mod Opt.CommandFields a
mkCmdPrs name desc prs =
  command name $
  info (helper <*> prs) $
  progDesc desc

cmdParser :: Opt.ParserInfo CliCommand
cmdParser = info (helper <*> cmdImpl) (progDesc exeDesc)
  where
    exeDesc :: String
    exeDesc = "Client for generic multisig contract for Tezos blockchain"

    cmdImpl :: Opt.Parser CliCommand
    cmdImpl = Opt.hsubparser . mconcat $
      [ printContractSubprs
      , printStorageSubprs
      , signSubprs
      , submitSubprs
      ]

    printContractSubprs =
      mkCmdPrs "print-contract" "Print multisig contract Michelson code" $
        pure PrintContract

    printStorageSubprs =
      mkCmdPrs "print-storage" "Print inital multisig contract storage" $
        PrintStorage <$> many pkHashArg
    msigCmdSub = Opt.hsubparser $ mconcat
      [ rotateKeysSubprs
      , proxyCallSubprs
      ]
    submitSubprs =
      mkCmdPrs "submit" "Submit an operation" $
        fmap Submit $ SubmitCliCommand
          <$> msigCmdSub
          <*> multisigContractOption
          <*> optional nonceOption
          <*> secretKeyConf
                "payer" "Secret key to withdraw transaction fee"
                "payer-alias"
                "Alias of the secret key to withdraw \
                  \transaction fee in tezos-client"
          <*> many sigPairOption
          <*> M.clientConfigParser (pure Nothing)

    signSubprs =
      mkCmdPrs "sign" "Sign an operation" $
        fmap Sign $ SignCliCommand
          <$> msigCmdSub
          <*> multisigContractOption
          <*> optional nonceOption
          <*> secretKeyConf
                "sk" "Secret key to sign contract call"
                "sk-alias"
                "Alias of the secret key to sign contract call in tezos-client"
          <*> M.clientConfigParser (pure Nothing)

    rotateKeysSubprs =
      mkCmdPrs "rotate-keys" "Print argument for msig to rotate keys" $
        RotateKeys <$> many pkHashArg

    proxyCallSubprs =
      mkCmdPrs "call" "Print argument for msig to make proxy call" $
         Call <$> (CallArgs <$> entrypointOption <*> paramOption <*> contractOption)



