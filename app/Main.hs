{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where

import EmurgoMinting qualified
import DAOValidator qualified 
import ParamApp qualified 
import Data.Default (
  def,
 )
import Ply.Plutarch (
  writeTypedScript,
 )
import Data.Text (
  Text,
  pack,
 )
import Plutarch (
  Config (Config),
  TracingMode (DoTracing),
  compile,
 )
import Plutarch.Evaluate (
  evalScript,
 )
import "liqwid-plutarch-extra" Plutarch.Extra.Script (
  applyArguments,
 )
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as Text
import Cardano.Binary qualified as CBOR
import Plutarch.Script (Script, serialiseScript)
import Plutarch.Prelude
import Data.Aeson (KeyValue ((.=)), object)
import Data.Aeson.Encode.Pretty (encodePretty)
import PlutusLedgerApi.V2 (
  Data,
  ExBudget,
 )
import Data.Bifunctor (
  first,
 )
encodeSerialiseCBOR :: Script -> Text
encodeSerialiseCBOR = Text.decodeUtf8 . Base16.encode . CBOR.serialize' . serialiseScript

evalT :: ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT x = evalWithArgsT x []

evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT x args = do
  cmp <- compile (Config DoTracing) x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

writePlutusScript :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScript title filepath term = do
  case evalT term of
    Left e -> putStrLn (show e)
    Right (script, _, _) -> do
      let
        scriptType = "PlutusScriptV2" :: String
        plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR script]
        content = encodePretty plutusJson
      LBS.writeFile filepath content

main :: IO ()
main = do
  -- strPutLn $ ParamApp.toSchema (DataParam {pkh = "deadbeef", password = "deadbeef"})
  writeTypedScript def "minting" "./compiled/emurgoMintingPolicy.plutus" EmurgoMinting.emurgoMintingPolicy
  writePlutusScript "multisig" "./compiled/multisigValidator.plutus" DAOValidator.emurgoDAOValidatorW
  writePlutusScript "metadata" "./compiled/metadataControl.plutus" EmurgoMinting.emurgoOnchainMetadataValidatorW
  writePlutusScript "multisigStateMint" "./compiled/multisigStateMint.plutus" DAOValidator.pvalidateDaoStateMintW
  --writePlutusScript "smallValidator" "./compiled/smallValidator.plutus" SmallValidator.pvalidateSmallChecksW