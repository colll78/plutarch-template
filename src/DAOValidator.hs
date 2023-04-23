{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module DAOValidator (emurgoDAOValidatorW) where

import Plutarch.Api.V2 
import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.DataRepr
import Plutarch.Api.V1.Value 
import Plutarch.Bool
import Plutarch.Prelude
import Plutarch.Extra.ScriptContext (ptryFromInlineDatum, pfromPDatum)
import Utils (ppositiveSymbolValueOf, (#>), (#>=))
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont 

pkh1 :: Term s PPubKeyHash 
pkh1 = pconstant "deadbeef1"

pkh2 :: Term s PPubKeyHash 
pkh2 = pconstant "deadbeef2"

pkh3 :: Term s PPubKeyHash 
pkh3 = pconstant "deadbeef3"

data PDaoAction (s :: S) = 
    Add (Term s (PDataRecord '["pkh" ':= PPubKeyHash]))
   | Remove (Term s (PDataRecord '["pkh" ':= PPubKeyHash]))
   | Approve (Term s (PDataRecord '[]))
 deriving stock (Generic)
 deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PDaoAction where
  type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData (PAsData PDaoAction)
instance PTryFrom PData PDaoAction 

data PDaoDatum (s :: S) = 
   PDaoDatum (Term s (PDataRecord '["approvedSignatories" ':= PBuiltinList (PAsData PPubKeyHash), "requiredNoOfSigs" ':= PInteger]))
 deriving stock (Generic)
 deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PDaoDatum where
  type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData (PAsData PDaoDatum)
instance PTryFrom PData PDaoDatum 

ptxSignedBy :: Term s (PBuiltinList (PAsData PPubKeyHash) :--> (PAsData PPubKeyHash) :--> PBool)
ptxSignedBy = phoistAcyclic $ plam $ \sigs pkh -> pelem # pkh # sigs 

paysToCredential :: Term s (PScriptHash :--> PTxOut :--> PBool)
paysToCredential = phoistAcyclic $
  plam $ \valHash txOut -> unTermCont $ do
    let txOutCred = pfield @"credential" # (pfield @"address" # txOut)
    pure $
      pmatch txOutCred $ \case
        PScriptCredential txOutValHash -> (pfield @"_0" # txOutValHash) #== valHash
        PPubKeyCredential _ -> (pcon PFalse)

ptryOwnInput :: (PIsListLike list PTxInInfo) => Term s (list PTxInInfo :--> PTxOutRef :--> PTxOut)
ptryOwnInput = phoistAcyclic $
  plam $ \inputs ownRef ->
    precList (\self x xs -> pletFields @'["outRef", "resolved"] x $ \txInFields -> pif (ownRef #== txInFields.outRef) txInFields.resolved (self # xs)) (const perror) # inputs

pscriptHashFromOut :: Term s (PTxOut :--> PScriptHash)
pscriptHashFromOut = phoistAcyclic $
  plam $ \out ->
   pmatch  (pfield @"credential" # (pfield @"address" # out)) $ \case 
    PPubKeyCredential _ -> perror 
    PScriptCredential ((pfield @"_0" #) -> cred) -> cred 

-- pelimList Arguments:
-- 1. function that returns something, runs when not empty
-- 2. nilCase -> what happens when list is empty    
-- 3. list to recurse on 

pheadSingleton :: (PListLike list, PElemConstraint list a) => Term s (list a :--> a)
pheadSingleton = phoistAcyclic $
  plam $ \xs ->
    pelimList (\x xs -> (pelimList (\_ _ -> perror) x xs)) perror xs 

-- Expand given list of conditions with pand' 
-- evalutates arguments strictly
pand'List :: [Term s PBool] -> Term s PBool
pand'List = foldr1 (\res x -> pand' # res # x)

emurgoValidator :: Term s (PCurrencySymbol :--> PDaoDatum :--> PDaoAction :--> PScriptContext :--> PUnit)
emurgoValidator = phoistAcyclic $ plam $ \stateCS dat redeemer ctx -> unTermCont $ do
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx

    PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose

    infoF <- pletFieldsC @'["inputs", "outputs", "signatories", "data"] ctxF.txInfo

    datF <- pletFieldsC @'["approvedSignatories", "requiredNoOfSigs"] dat 
    let ownInput = ptryOwnInput # infoF.inputs # ownRef 
    ownInputF <- pletFieldsC @'["value", "address"] ownInput 
    PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatchC $ pfield @"credential" # ownInputF.address
    let ownOutput = pheadSingleton #$ pfilter # (paysToCredential # ownValHash) # infoF.outputs 
    ownOutputF <- pletFieldsC @'["value", "datum"] ownOutput
    let outDatum = pfromPDatum @PDaoDatum # (ptryFromInlineDatum # ownOutputF.datum)
    outDatumF <- pletFieldsC @'["approvedSignatories", "requiredNoOfSigs"] outDatum 
    newApprovedSigs :: Term _ (PBuiltinList (PAsData PPubKeyHash)) <- pletC $ pfromData outDatumF.approvedSignatories 
    sigs :: Term _ (PBuiltinList (PAsData PPubKeyHash)) <- pletC infoF.signatories 
    
    stateTn <- pletC (pcon (PTokenName (pto ownValHash)))

    pure $ 
      pif 
        (pvalueOf # ownInputF.value # stateCS # stateTn #== 1 
          #&& pvalueOf # ownOutputF.value # stateCS # stateTn #== 1
          #&& 
            (pmatch redeemer $ \case 
                Add r -> 
                  plet (plength # newApprovedSigs) $ \outLength ->
                    plet (pfield @"pkh" # r) $ \pkh ->
                      pand'List 
                        [ (plength # (pfilter # plam (\x -> pelem # x # datF.approvedSignatories) # sigs) #>= datF.requiredNoOfSigs)
                        , pfromData ownInputF.value #<= pfromData ownOutputF.value
                        , outLength #== (plength @PBuiltinList @(PAsData PPubKeyHash) # datF.approvedSignatories) + 1
                        , plistEquals # (ptail # newApprovedSigs) # datF.approvedSignatories
                        , pnot #$ pelem # (phead # newApprovedSigs) # (ptail # newApprovedSigs)
                        , pkh #== (phead # newApprovedSigs)
                        , outLength #> pfromData outDatumF.requiredNoOfSigs
                        , outLength #< 8 
                        ]
                Remove r ->
                  plet (plength # newApprovedSigs) $ \outLength ->
                    plet (pfield @"pkh" # r) $ \pkh ->
                      (plength # (pfilter # plam (\x -> pelem # x # datF.approvedSignatories) # sigs) #>= datF.requiredNoOfSigs)
                        #&& pfromData ownInputF.value #<= pfromData ownOutputF.value 
                        #&& outLength #== (plength # pfromData datF.approvedSignatories) - 1
                        #&& pall # plam (\x -> pelem # x # newApprovedSigs #|| pkh #== x) # datF.approvedSignatories
                        #&& pelem # pkh # datF.approvedSignatories
                        #&& pnot # (pelem # pkh # newApprovedSigs)
                        #&& outLength #> pfromData outDatumF.requiredNoOfSigs
                Approve _ -> 
                  plength # (pfilter # plam (\x -> pelem # x # datF.approvedSignatories) # sigs) #>= datF.requiredNoOfSigs
                    #&& datF.requiredNoOfSigs #== outDatumF.requiredNoOfSigs
                    #&& plistEquals # pfromData datF.approvedSignatories # newApprovedSigs
            )
          )
          (pconstant ())
          perror 

emurgoDAOValidatorW :: ClosedTerm (PCurrencySymbol :--> PValidator)
emurgoDAOValidatorW = plam $ \cs datum redeemer ctx -> unTermCont $ do 
  (dat, _) <- ptryFromC @PDaoDatum datum 
  (redmr, _) <- ptryFromC @PDaoAction redeemer 
  pure $ popaque $ emurgoValidator # cs # dat # redmr # ctx

psingletonTokenNameWithCS :: 
  forall 
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S). 
  Term s (PAsData PCurrencySymbol :--> PValue keys amounts :--> PTokenName)
psingletonTokenNameWithCS = phoistAcyclic $ plam $ \policyId val ->
  pmatch val $ \(PValue val') ->
    precList 
      (\self x xs ->
        pif 
          (pfstBuiltin # x #== policyId)
          ( pmatch (pfromData (psndBuiltin # x)) $ \(PMap tokens) ->
              plet (phead # tokens) $ \fstTk ->
                (pif 
                  (pnull # (ptail # tokens) #&& pfromData (psndBuiltin # fstTk) #== 1)
                  (pfromData $ pfstBuiltin # fstTk)
                  perror 
                )
          )
          (self # xs)
        )
      (const perror)
      # pto val'

pvalidateDaoStateMint :: Term s (PTxOutRef :--> PScriptContext :--> PUnit)
pvalidateDaoStateMint = phoistAcyclic $ plam $ \oref context -> unTermCont $ do 
  ctxF <- pletFieldsC @'["txInfo", "purpose"] context 
  PMinting ((pfield @"_0" #) -> policy) <- pmatchC ctxF.purpose
  ownPolicyId <- pletC policy 
  infoF <- pletFieldsC @'["inputs", "outputs", "mint"] ctxF.txInfo 
  let isUTxOSpent = pany @PBuiltinList 
                      # plam
                        ( \txIn ->
                            let txInRef = pfield @"outRef" # txIn
                            in txInRef #== oref
                        )
                      # infoF.inputs
  mintedTn <- pletC $ psingletonTokenNameWithCS # pdata ownPolicyId # infoF.mint 
  txOutputs :: Term _ (PBuiltinList PTxOut) <- pletC infoF.outputs 

  let mintsToVh = 
        pany @PBuiltinList
          # plam (\txo -> 
                    pletFields @'["address", "datum", "value"] txo $ \txoF ->
                      pmatch (pfield @"credential" # txoF.address) $ \case 
                        PScriptCredential ((pfield @"_0" #) -> cvh) -> 
                          let outDatum = pfromPDatum @PDaoDatum # (ptryFromInlineDatum # txoF.datum)
                           in pletFields @'["requiredNoOfSigs", "approvedSignatories"] outDatum $ \outDF ->
                                pfromData (pto cvh) #== pto mintedTn
                                  #&& pvalueOf # txoF.value # ownPolicyId # mintedTn #== 1
                                  #&& (plength @PBuiltinList # outDF.approvedSignatories) #>= outDF.requiredNoOfSigs
                        PPubKeyCredential _ -> pconstant False 
                  )
          # txOutputs 
  
  pure $
    pif 
      ( isUTxOSpent 
          #&& mintsToVh
          #&& ppositiveSymbolValueOf # ownPolicyId # infoF.mint #== 1
      )   
      (pconstant ())
      perror 
  
  