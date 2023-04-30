{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NumericUnderscores    #-}

module DAOValidator where

-- import           Data.Aeson          (ToJSON, FromJSON)
import           GHC.Generics (Generic)
import qualified PlutusTx
import qualified PlutusTx.AssocMap         as M
import           PlutusTx.Prelude
import           Plutus.Script.Utils.Typed (mkUntypedValidator)  -- (mkUntypedMintingPolicy)
import qualified Ledger                    as L
import           Plutus.V1.Ledger.Value
import           Plutus.V2.Ledger.Api
-- import           Plutus.V2.Ledger.Contexts as V2LC
import           Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2V

pkh1 :: L.PaymentPubKeyHash
pkh1 = L.PaymentPubKeyHash . PubKeyHash $ "deadbeef1"

pkh2 :: L.PaymentPubKeyHash
pkh2 = L.PaymentPubKeyHash . PubKeyHash $ "deadbeef2"

pkh3 :: L.PaymentPubKeyHash
pkh3 = L.PaymentPubKeyHash . PubKeyHash $ "deadbeef3"

data DaoDatum = DaoDatum
  { approvedSignatories :: [L.PaymentPubKeyHash]
  , requiredNoOfSigs :: Integer
  }
  deriving Generic

PlutusTx.unstableMakeIsData ''DaoDatum

data DaoAction = Add L.PaymentPubKeyHash
                 | Remove L.PaymentPubKeyHash
                 | Approve [L.PaymentPubKeyHash]
  deriving Generic

PlutusTx.unstableMakeIsData ''DaoAction

{-# INLINABLE getDaoDatum #-}
getDaoDatum :: Datum -> Maybe DaoDatum
getDaoDatum = PlutusTx.fromBuiltinData . getDatum

paysToCredential :: ValidatorHash -> TxOut -> Bool
paysToCredential valHash txOut =
  let
    txOutCred = addressCredential . txOutAddress $ txOut
  in
    case txOutCred of
      ScriptCredential txOutValHash -> txOutValHash == valHash
      PubKeyCredential _            -> False

tryOwnInput :: [TxInInfo] -> TxOutRef -> TxOut
tryOwnInput inputs ownRef =
  let
    xs   = filter (\x -> ownRef == (txInInfoOutRef x)) inputs
    self = case xs of
      [_] -> head
      _   -> traceError "error"
  in
    txInInfoResolved (self xs)

scriptHashFromOut :: TxOut -> ValidatorHash
scriptHashFromOut out = case addressCredential . txOutAddress $ out of
  PubKeyCredential _    -> traceError "error"
  ScriptCredential cred -> cred

headSingleton ::[a] -> a
headSingleton (x : xs) = if null xs
  then x
  else traceError "List contains more than one element."
headSingleton [] = traceError "error"

tryFromInlineDatum :: OutputDatum -> Datum
tryFromInlineDatum outdat = case outdat of
  OutputDatum dat -> dat
  _               -> traceError "error"

positiveSymbolValueOf :: CurrencySymbol -> Value -> Bool
positiveSymbolValueOf cs v = case M.lookup cs (getValue v) of
  Nothing -> False
  Just csTokens -> M.null $ M.filter (<0) csTokens

emurgoValidator :: CurrencySymbol -> DaoDatum -> DaoAction -> ScriptContext -> Bool
emurgoValidator stateCS dat redeemer ctx =
  let
    info = scriptContextTxInfo ctx
    ownRef = case scriptContextPurpose ctx of
      Spending oref -> oref
      _             -> traceError "error"
    ownInput = tryOwnInput (txInfoInputs info) ownRef
    ownValHash = case addressCredential . txOutAddress $ ownInput of
      ScriptCredential vh -> vh
      _                   -> traceError "error"
    -- ToDo: define 'paysToCredential'
    ownOutput = headSingleton $ filter (paysToCredential ownValHash) (txInfoOutputs info)
    sigs = L.PaymentPubKeyHash <$> txInfoSignatories info
    stateTn = (\(ValidatorHash b) -> TokenName b) $ ownValHash
  in
    case txOutDatum ownOutput of
      OutputDatum outDatum' ->
        let
          outDatum = case getDaoDatum outDatum' of
            Nothing -> traceError "error"
            Just d  -> d :: DaoDatum
          newApprovedSigs = approvedSignatories outDatum
        in
          -- ToDo: define stateCS
          valueOf (txOutValue ownInput) stateCS stateTn  == 1 &&
          valueOf (txOutValue ownOutput) stateCS stateTn == 1 &&
          (case redeemer of
             Add r     ->
               let
                 outLength = length newApprovedSigs
                 pkh       = r
               in
                 length (filter (\x -> elem x (approvedSignatories dat)) sigs) >= requiredNoOfSigs dat &&
                 (txOutValue ownInput) `leq` (txOutValue ownOutput)                                    &&  -- this seems odd
                 outLength == (length $ approvedSignatories dat) + 1                                   &&
                 tail newApprovedSigs == approvedSignatories dat                                       &&
                 (not $ elem (head newApprovedSigs) (tail newApprovedSigs))                            &&
                 pkh == head newApprovedSigs                                                           &&
                 outLength > requiredNoOfSigs outDatum                                                 &&
                 outLength < 8
             Remove r  ->
               let
                 outLength = length newApprovedSigs
                 pkh       = r
               in
                 length (filter (\x -> elem x (approvedSignatories dat)) sigs) >= requiredNoOfSigs dat &&
                 (txOutValue ownInput) `leq` (txOutValue ownOutput)                                    && -- this seems odd
                 outLength == (length $ approvedSignatories dat) - 1                                   &&
                 all (\x -> elem x newApprovedSigs || x == pkh) (approvedSignatories dat)              &&
                 elem pkh (approvedSignatories dat)                                                    &&
                 (not $ elem pkh newApprovedSigs)                                                      &&
                 outLength > requiredNoOfSigs outDatum
             Approve _ ->
               length (filter (\x -> elem x (approvedSignatories dat)) sigs) >= requiredNoOfSigs dat &&
               requiredNoOfSigs dat == requiredNoOfSigs outDatum                                     &&
               approvedSignatories dat == newApprovedSigs
          )
      _                    -> traceError "error"

data TypedEmurgoVal
instance V2V.ValidatorTypes TypedEmurgoVal where
  type instance DatumType TypedEmurgoVal    = DaoDatum
  type instance RedeemerType TypedEmurgoVal = DaoAction

emurgoValidatorT :: CurrencySymbol -> V2V.TypedValidator TypedEmurgoVal
emurgoValidatorT cs = V2V.mkTypedValidator @TypedEmurgoVal
  ($$(PlutusTx.compile [|| emurgoValidator ||])
     `PlutusTx.applyCode` PlutusTx.liftCode cs)
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = mkUntypedValidator @ScriptContext @DaoDatum @DaoAction

singletonTokenNameWithCS :: CurrencySymbol -> Value -> TokenName
singletonTokenNameWithCS policyId val = case M.lookup policyId (getValue val) of
  Nothing -> traceError "error"
  Just tokens -> case M.toList tokens of
    [fstTk] -> if snd fstTk == 1
      then fst fstTk
      else traceError "error"
    _       -> traceError "error"

validateDaoStateMint :: TxOutRef -> ScriptContext -> Bool
validateDaoStateMint oref context =
  let
    ownPolicyId = case scriptContextPurpose context of
      Minting policy' -> policy'
      _               -> traceError "error"
    info           = scriptContextTxInfo context
    isUTxOSpent    = any (\txIn ->
                         let txInRef = txInInfoOutRef txIn
                         in  txInRef == oref
                      ) (txInfoInputs info)
    mintedTn  = singletonTokenNameWithCS ownPolicyId (txInfoMint info)
    txOutputs = txInfoOutputs info
    mintsToVh = any (\txo -> case addressCredential $ txOutAddress txo of
                        ScriptCredential cvh ->
                          let
                            outDatum = case getDaoDatum . tryFromInlineDatum . txOutDatum $ txo of
                              Nothing        -> traceError "error"
                              Just outDatum' -> outDatum'
                            ValidatorHash cvh' = cvh
                          in
                            cvh' == unTokenName mintedTn                                       &&
                            valueOf (txOutValue txo) ownPolicyId mintedTn == 1                 &&
                            length (approvedSignatories outDatum) >= requiredNoOfSigs outDatum
                        PubKeyCredential _   -> traceError "error"
                    ) txOutputs
  in
    isUTxOSpent                                         &&
    mintsToVh                                           &&
    positiveSymbolValueOf ownPolicyId (txInfoMint info)