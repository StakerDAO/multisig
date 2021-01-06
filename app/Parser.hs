module Parser
  ( cmdParser
  , CallArgs (..)
  , SignedCommand (..)
  , CliCommand (..)
  ) where

import Universum

import Michelson.Untyped as M
import qualified Options.Applicative as Opt

import Options.Applicative (command, helper, info, progDesc)
import Tezos.Crypto (KeyHash)

import Lorentz.Contracts.Multisig (Signatures)

import Options

data CallArgs = CallArgs
  { caEntrypoint :: M.EpName
  , caParam :: M.Value
  }

data SignedCommand
  = RotateKeys [KeyHash]
  | Call CallArgs

data CliCommand
  = PrintContract
  | PrintStorage [KeyHash]
  | WithSigs (Maybe Signatures) Natural SignedCommand

mkCmdPrs
  :: String
  -> String
  -> Opt.Parser a
  -> Opt.Mod Opt.CommandFields a
mkCmdPrs name desc prs =
  command name $
  info (helper <*> prs) $
  progDesc desc

mkSignedCmdPrs
  :: String -> String
  -> Opt.Parser SignedCommand
  -> Opt.Mod Opt.CommandFields CliCommand
mkSignedCmdPrs name desc innerPrs =
  command name $
    info (helper <*> (WithSigs <$>  many sigPairOpt <*> nonceOpt <*> innerPrs)) $
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
      , rotateKeysSubprs
      , proxyCallSubprs
      ]

    printContractSubprs =
      mkCmdPrs "print-contract" "Print multisig contract Michelson code" $
        pure PrintContract

    printStorageSubprs =
      mkCmdPrs "print-storage" "Print inital multisig contract storage" $
        PrintStorage <$> many pkHashArg

    rotateKeysSubprs =
      mkSignedCmdPrs "rotate-keys" "Print argument for msig to rotate keys" $
        RotateKeys <$> many pkHashArg

    proxyCallSubprs =
      mkSignedCmdPrs "proxy-call" "Print argument for msig to make proxy call" $
         Call <$> (CallArgs <$> entrypointOpt <*> paramOption)



