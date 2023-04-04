module Main (main) where

import Data.Default (
  def,
 )
import EmurgoMinting qualified
import Ply.Plutarch (
  writeTypedScript,
 )

main :: IO ()
main = do
  writeTypedScript def "test" ("./compiled/emurgoMintingPolicy.plutus") EmurgoMinting.emurgoMintingPolicy
