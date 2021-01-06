module Main
  ( main
  ) where

import Universum

import qualified Lorentz as L
import qualified Lorentz.Contracts.Multisig as Msig
import qualified Options.Applicative as Opt

import Data.Set (fromList)

import Parser

main :: IO ()
main = Opt.execParser cmdParser >>= \case
  PrintContract -> putStr $ L.printLorentzContract False $
    L.defaultContract Msig.multisigContract
  PrintStorage keyHashes -> putStr $ L.printLorentzValue False $
    Msig.Storage (fromList keyHashes) 0
  WithSigs sigs nonce signedCmd ->
    let printParam order = putStr $ L.printLorentzValue False $
          Msig.Parameter { signatures = sigs, .. }
    in printParam $ case signedCmd of
      RotateKeys keyHashes -> Msig.mkRotateKeysOrder (fromList keyHashes)
      Call CallArgs{..} -> error "keks"

