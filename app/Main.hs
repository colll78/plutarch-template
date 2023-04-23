module Main (main) where

import EmurgoMinting qualified
import DAOValidator qualified 
import Data.Default (
  def,
 )
import Ply.Plutarch (
  writeTypedScript,
 )

main :: IO ()
main = do
  writeTypedScript def "test" ("./compiled/emurgoMintingPolicy.plutus") EmurgoMinting.emurgoMintingPolicy
  writeTypedScript def "multisig" ("./compiled/multisigValidator.plutus") DAOValidator.emurgoDAOValidatorW
