{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Basics where

import Plutarch 
import Plutarch.Api.V2 
import Plutarch.Prelude 
import Plutarch.Monadic qualified as P 
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont 
import Utils (pand'List, pcond, (#>), (#>=), ptryOwnInput, pcountScriptInputs)
import PlutusLedgerApi.V1
import Plutarch.Api.V1.Value
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Unsafe
import Plutarch.DataRepr (PDataFields)

protocolFeeTarget :: Term s PAddress
protocolFeeTarget = pconstant (Address (PubKeyCredential (PubKeyHash "deadbeef")) Nothing)

projectKey :: Term s PPubKeyHash 
projectKey = pconstant "deadbeef"


-- Unlocking conditions for each input UTxO:
--  Transaction must be signed by projectKey
--  25% of the ADA locked in the UTxO must be sent to protocolFeeTarget

basicValidator :: Term s PValidator 
basicValidator = phoistAcyclic $ plam $ \_dat _redeemer ctx -> P.do 
  ctxF <- pletFields @'["txInfo", "purpose"] ctx
  infoF <- pletFields @'["signatories", "inputs", "outputs"] ctxF.txInfo 

  PSpending ((pfield @"_0" #) -> ownRef) <- pmatch ctxF.purpose 
  
  let ownInput = ptryOwnInput # infoF.inputs # ownRef
      thresholdOwed = pdiv # (plovelaceValueOf # (pfield @"value" # ownInput)) # 4
      paymentOut = 
        pfind @PBuiltinList 
          # plam (\out -> 
              (pfield @"address" # out) #== protocolFeeTarget 
                #&& plovelaceValueOf # (pfield @"value" # out) #>= thresholdOwed
            ) 
          # infoF.outputs
      hasPaymentOutput = pmatch paymentOut $ \case 
        PNothing -> pconstant False 
        PJust _ -> pconstant True

  pif (hasPaymentOutput #&& pelem # pdata projectKey # infoF.signatories)
      (popaque (pconstant ()))
      perror 


basicValidatorImproved :: Term s (PStakingCredential :--> PValidator)
basicValidatorImproved = phoistAcyclic $ plam $ \stakeCred _dat _redeemer ctx -> P.do
  ctxF <- pletFields @'["txInfo"] ctx 
  infoF <- pletFields @'["wdrl"] ctxF.txInfo 

  let hasCred = pmatch (plookup # stakeCred # infoF.wdrl) $ \case 
        PNothing -> pconstant False 
        PJust _ -> pconstant True 
  
  pif (hasCred)
      (popaque $ pconstant ())
      perror 

data PGlobalRedeemer (s :: S) = PGlobalRedeemer 
  (Term s (PDataRecord 
    '[ "inputIdxs" ':= PBuiltinList (PAsData PInteger)
     , "outputIdxs" ':= PBuiltinList (PAsData PInteger) 
     ]
  ))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PGlobalRedeemer where 
    type DPTStrat _ = PlutusTypeData

globalLogicBasic :: Term s PStakeValidator 
globalLogicBasic = phoistAcyclic $ plam $ \_red ctx -> P.do 
  let red = punsafeCoerce @_ @_ @PGlobalRedeemer _red 
  redF <- pletFields @'["inputIdxs", "outputIdxs"] red 
  ctxF <- pletFields @'["txInfo"] ctx 
  infoF <- pletFields @'["inputs", "outputs", "signatories"] ctxF.txInfo 

  let scInputs = pmap @PBuiltinList # plam (\idx -> pfield @"resolved" #$ pelemAt @PBuiltinList # pfromData idx # infoF.inputs) # redF.inputIdxs  
  --    scOutputs = pmap @PBuiltinList # plam (\idx -> pelemAt @PBuiltinList # pfromData idx # infoF.inputs) # redF.outputIdxs 
  
  let adaInInputs = pfoldl # plam (\acc txIn -> acc + plovelaceValueOf # (pfield @"value" # txIn) ) # 0 # scInputs 
      thresholdOwed = pdiv # adaInInputs # 4 
      paymentOut = 
        pfind @PBuiltinList 
          # plam (\out -> 
              (pfield @"address" # out) #== protocolFeeTarget 
                #&& plovelaceValueOf # (pfield @"value" # out) #>= thresholdOwed
            ) 
          # infoF.outputs
      hasPaymentOutput = pmatch paymentOut $ \case 
        PNothing -> pconstant False 
        PJust _ -> pconstant True

  let checks =
        pand'List 
          [ pcountScriptInputs # infoF.inputs #== plength @PBuiltinList # redF.inputIdxs 
          , hasPaymentOutput
          , (pelem # pdata projectKey # infoF.signatories #&& hasPaymentOutput)
          ]

  pif checks 
      (popaque (pconstant ()))
      perror 