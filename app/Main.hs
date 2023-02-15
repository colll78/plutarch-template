module Main (main) where

import AlwaysSucceeds qualified
import Data.Default (
  def,
 )
import Ply.Plutarch (
  writeTypedScript,
 )

main :: IO ()
main = do
  writeTypedScript def "test" "./alwaysSucceeds.plutus" AlwaysSucceeds.validator
