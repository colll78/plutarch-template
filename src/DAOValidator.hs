{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ViewPatterns #-}
module DAOValidator (emurgoDAOValidator) where

import Plutarch.Api.V2 
import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.DataRepr
import Plutarch.Api.V1.Value 
import Plutarch.Prelude
import Plutarch.Extra.ScriptContext (ptryFromInlineDatum, pfromPDatum)

pkh1 :: Term s PPubKeyCredential 
pkh1 = pconstant "deadbeef1"

pkh2 :: Term s PPubKeyCredential 
pkh2 = pconstant "deadbeef2"

pkh3 :: Term s PPubKeyCredential 
pkh3 = pconstant "deadbeef3"

data PDaoAction (s :: S) = 
    Add (Term s (PDataRecord '["pkh" ':= PPubKeyCredential]))
   | Remove (Term s (PDataRecord '["pkh" ':= PPubKeyCredential]))
   | Approve (Term s (PDataRecord '[]))
 deriving stock (Generic, Enum, Bounded)
 deriving anyclass (PlutusType, PIsData, PEq, PShow)

data PDaoDatum (s :: S) = 
   PDaoDatum (Term s (PDataRecord '["approvedSignatories" ':= PBuiltinList PPubKeyCredential, "requiredNoOfSigs" ':= PInteger]))
 deriving stock (Generic)
 deriving anyclass (PlutusType, PIsData, PShow)

ptxSignedBy :: Term s (PBuiltinList PPubKeyCredential :--> PPubKeyCredential :--> PBool)
ptxSignedBy = phoistAcyclic $ plam $ \sigs pkh -> pelem # pkh # sigs 

paysToCredential :: Term s (PValidatorHash :--> PTxOut :--> PBool)
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

pscriptHashFromOut :: (PIsListLike list PTxInInfo) => Term s (PTxOut :--> PScriptHash)
pscriptHashFromOut = phoistAcyclic $
  plam $ \out ->
   pmatch (pfield @"address" # out) $ \case 
    PPubKeyHash _ -> perror 
    PScriptCredential ((pfield @"_0" #) -> cred) -> cred 

pheadSingleton :: (PListLike list, PElemConstraint list a) => Term s (list a :--> a)
pheadSingleton = phoistAcyclic $
  plam $ \xs ->
    pelimList (\x xs -> pif (pnull # xs) x (ptraceError "List contains more than one element.")) perror xs


emurgoValidator :: Term s (PCurrencySymbol :--> PDaoDatum :--> PDaoAction :--> PScriptContext :--> PUnit)
emurgoValidator = phoistAcyclic $ plam $ \stateCS dat redeemer ctx -> unTermCont $ do
    ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx

    PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose

    infoF <- pletFieldsC @'["inputs", "outputs", "signatories", "data"] ctxF.txInfo

    datF <- pletFieldsC @'["approvedSignatories", "requiredNoOfSigs"] dat 
    let ownInput = ptryOwnInput # infoF.inputs # ownRef 
    ownInputF <- pletFields @'["value", "address"] ownInput 
    PScriptCredential ((pfield @"_0" #) -> ownValHash) <- pmatchC $ pfield @"credential" # ownInputF.address
    let ownOutput = pheadSingleton #$ pfilter # (paysToCredential # ownValHash) # infoF.outputs 
    ownOutputF <- pletFields @'["value", "datum"] # ownOutput
    let outDatum = pfromPDatum @PDaoDatum # (ptryFromInlineDatum # ownOutputF.datum)
    outDatumF <- pletFields @'["approvedSignatories", "requiredNoOfSigs"] outDatum 
    
    sigs <- pletC infoF.signatories 
    
    stateTn <- pletC (pcon PTokenName (pto ownValHash))


    pure $ 
      pif 
        (pvalueOf # ownInputF.value # stateCS # stateTn #== 1 
          #&& pvalueOf # ownOutputF.value # stateCS # stateTn #== 1
          #&& 
            pmatch redeemer $ \case 
                Add r -> 
                  plet (plength # outDatumF.approvedSignatories) $ \outLength ->
                    (plength # (pfilter # plam (\x -> pelem # x # datF.approvedSignatories) # sigs) #>= datF.requiredNoOfSigs)
                        #&& ownInputF.value #<= ownOutputF.value 
                        #&& outLength #== (plength # datF.approvedSignatories) + 1
                        #&& plistEquals # (ptail # outDatumF.approvedSignatories) # datF.approvedSignatories
                        #&& pnot $# pelem # (phead # outDatumF.approvedSignatories) # outDatumF.approvedSignatories
                        #&& outLength #> outDatumF.requiredNoOfSigs
                        #&& outLength #< 8 
                Remove r ->
                  plet (plength # outDatumF.approvedSignatories) $ \outLength ->
                    plet (pfield @"pkh" # r) $ \pkh ->
                      (plength # (pfilter # plam (\x -> pelem # x # datF.approvedSignatories) # sigs) #>= datF.requiredNoOfSigs)
                          #&& ownInputF.value #<= ownOutputF.value 
                          #&& outLength #== (plength # datF.approvedSignatories) - 1
                          #&& pall # plam (\x -> pelem # x # outDatumF.approvedSignatories) # datF.approvedSignatories
                          #&& pelem # pkh # datF.approvedSignatories
                          #&& pnot #$ pelem # pkh # outDatumF.approvedSignatories
                          #&& outLength #> outDatumF.requiredNoOfSigs
                Approve _ -> 
                  plength # (pfilter # plam (\x -> pelem # x # datF.approvedSignatories) # sigs) #>= datF.requiredNoOfSigs
                    #&& datF.requiredNoOfSigs #== outDatumF.requiredNoOfSigs
                    #&& plistEquals # datF.approvedSignatories # outDatumF.approvedSignatories
          )
          (pconstant ())
          perror 


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
        )


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
  mintedTn <- psingletonTokenNameWithCS # pdata ownPolicyId # infoF.mint 
  txOutputs <- pletC infoF.outputs 

  let mintsToVh = 
    pany 
      # plam 
        (\txo -> 
          pletFields @'["address", "datum", "value"] $\txoF ->
            pmatch (pfield @"credential" # txoF.address) $ \case 
              PScriptCredential ((pfield @"_0" #) -> cvh) -> 
                let outDatum = pfromPDatum @PDaoDatum # (ptryFromInlineDatum # txoF.datum)
                 in pletFields @'["requiredNoOfSigs", "approvedSignatories"] outDatum $ \outDF ->
                  pto cvh #== pto mintedTn
                      #&& pvalueOf # txoF.value # ownPolicyId # mintedTn #== 1
                      #&& plength # outDF.approvedSignatories #>= outDF.requiredNoOfSigs
              PPubKeyCredential _ -> pconstant False 
        )
      # txOutputs 
  pure $
    pif 
      ( isUTxOSpent 
          #&& mintsToVh
      )   
  
  