{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module EmurgoMinting (emurgoMintingPolicy, emurgoOnchainMetadataValidatorW) where

import Plutarch.Api.V2 
import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.DataRepr
import Plutarch.Lift 
import Plutarch.Api.V1.Value 
import Plutarch.Prelude
import Plutarch.Extra.IsData (EnumIsData (..), PlutusTypeEnumData)
import Plutarch.Extra.Value (psymbolValueOf')
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont 
import "liqwid-plutarch-extra" Plutarch.Extra.ScriptContext (pfromPDatum)
import Utils (pand'List, pcond)
import PlutusTx qualified 
import PlutusTx.AssocMap qualified as AssocMap 
import PlutusLedgerApi.V1 (BuiltinData(..), Value(..), BuiltinByteString)
-- data MintAction = Mint   
--                 | Burn 

-- PMint: Constr 0 []
-- PBurn: Constr 1 []
-- data PMintAction' (s :: S) = 
--    PMint (Term s (PDataRecord '[]))
--  | PBurn (Term s (PDataRecord '[]))
--  deriving stock (Generic)
--  deriving anyclass (PlutusType, PIsData, PShow)

-- instance DerivePlutusType PMintAction' where
--   type DPTStrat _ = PlutusTypeData 

-- PMint: 0
-- PBurn: 1 

data PMintAction (s :: S) = PMint | PBurn
 deriving stock (Generic, Enum, Bounded)
 deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PMintAction where
  type DPTStrat _ = PlutusTypeEnumData 

instance PTryFrom PData (PAsData PMintAction)

data CreditScoreInfo = CreditScoreInfo 
  { creditScore :: Integer
  , numCompleted :: Integer 
  , numDefaults :: Integer 
  , totalCollateral :: Value 
  , totalValueBorrowed :: Value 
  , totalValueLent :: Value 
  , totalValuePaidBack :: Value 
  , totalValueDefaulted :: Value
  }
PlutusTx.makeLift ''CreditScoreInfo
PlutusTx.makeIsDataIndexed ''CreditScoreInfo[('CreditScoreInfo, 0)]

-- Constr 0 [Data, Integer, Data]
data DatumMetadata = DatumMetadata { metadata :: BuiltinData, version :: Integer, extra :: CreditScoreInfo}
PlutusTx.makeLift ''DatumMetadata
PlutusTx.makeIsDataIndexed ''DatumMetadata [('DatumMetadata, 0)]


data PCreditScoreInfo ( s :: S ) =
  PCreditScoreInfo (Term s (PDataRecord 
    '[ "creditScore" ':= PInteger
     , "numCompleted" ':= PInteger
     , "numDefaults" ':= PInteger
     , "totalCollateral" ':= PValue 'Sorted 'Positive 
     , "totalValueBorrowed" ':= PValue 'Sorted 'Positive
     , "totalValueLent" ':= PValue 'Sorted 'Positive
     , "totalValuePaidBack" ':= PValue 'Sorted 'Positive
     , "totalValueDefaulted" ':= PValue 'Sorted 'Positive
     ]
     ))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PCreditScoreInfo where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PCreditScoreInfo 

instance PUnsafeLiftDecl PCreditScoreInfo where type PLifted PCreditScoreInfo = CreditScoreInfo
deriving via (DerivePConstantViaData CreditScoreInfo PCreditScoreInfo) instance PConstantDecl CreditScoreInfo

data PMintMetadataDatum ( s :: S ) =
  PMintMetadataDatum (Term s (PDataRecord '["metadata" ':= PData, "version" ':= PInteger, "extra" ':= PCreditScoreInfo]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType PMintMetadataDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PMintMetadataDatum 

instance PUnsafeLiftDecl PMintMetadataDatum where type PLifted PMintMetadataDatum = DatumMetadata
deriving via (DerivePConstantViaData DatumMetadata PMintMetadataDatum) instance PConstantDecl DatumMetadata

enforcedNFTMetadata :: Term s PMintMetadataDatum 
enforcedNFTMetadata = pconstant nftMetadataDatum 
  where
    nftMetadataDatum :: DatumMetadata 
    nftMetadataDatum = DatumMetadata {metadata = PlutusTx.toBuiltinData enforcedMetadata, version = 0, extra = creditScoreInfo}

    creditScoreInfo :: CreditScoreInfo 
    creditScoreInfo = CreditScoreInfo
      { creditScore = 0 
      , numCompleted = 0 
      , numDefaults = 0  
      , totalCollateral = mempty 
      , totalValueBorrowed = mempty  
      , totalValueLent = mempty 
      , totalValuePaidBack = mempty 
      , totalValueDefaulted = mempty
      }
    enforcedMetadata :: AssocMap.Map BuiltinByteString BuiltinByteString 
    enforcedMetadata = AssocMap.fromList 
      [ ("name", "Credit Score Report")
      , ("image", "ipfs://QmSAkzP1DTvGe6n2EQB45LWN7wkT2hKDbFc8oga1LcHsvx")
      ]


treasuryValHash :: Term s PScriptHash 
treasuryValHash = pconstant "791d504ef072be11979a3c722bf1a46a8af77be7326fb7949da497c1"

metadataControlValHash :: Term s PScriptHash 
metadataControlValHash = pconstant "791d504ef072be11979a3c722bf1a46a8af77be7326fb7949da497c2"

mintingCost :: Term s PInteger 
mintingCost = 75_000_000

-- Reference NFT Label
label100 :: Term s PByteString 
label100 = phexByteStr "000643b0" -- 6

-- NFT Label
label222 :: Term s PByteString 
label222 = phexByteStr "000de140" -- 13

ptryLookupValue :: 
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S). 
  Term s 
    ( PAsData PCurrencySymbol
        :--> PValue keys amounts 
        :--> (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
    )
ptryLookupValue = phoistAcyclic $ plam $ \policyId val ->
  let valItems = pto (pto val)
   in (pfix #$ plam $ \self xs ->
        pelimList
          ( \y ys ->
              pif
                (policyId #== (pfstBuiltin # y))
                (pto (pfromData (psndBuiltin # y)))
                (self # ys)
          )
          perror
          xs
        )
        # valItems 

pbreakTokenName :: Term s PTokenName -> Term s (PPair PByteString PByteString)
pbreakTokenName tn = 
  let tnBS = pto tn 
   in pcon $ PPair (psliceBS # 0 # 4 # tnBS) (psliceBS # 4 # (plengthBS # tnBS) # tnBS)

-- Credit Score NFT 
emurgoMintingPolicyT :: Term s (PTxOutRef :--> PMintAction :--> PScriptContext :--> PUnit)
emurgoMintingPolicyT = phoistAcyclic $ plam $ \oref redeemer ctx -> unTermCont $ do
  ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx 
  infoF <- pletFieldsC @'["mint", "outputs", "inputs"] ctxF.txInfo -- Value [(cs, [(tnA, -1), (tnB, 1)])]

  PMinting purpose <- pmatchC ctxF.purpose 
  ownCS <- pletC (pfield @"_0" # purpose)
  
  mintedItems <- pletC $ ptryLookupValue # ownCS # infoF.mint
  refTokenPair <- pletC $ phead # mintedItems 
  userTokenPair <- pletC $ ptryIndex 1 mintedItems 

  let txOutputs :: Term _ (PBuiltinList PTxOut) 
      txOutputs = infoF.outputs  
      refTokenName = pfromData $ pfstBuiltin # refTokenPair
      refTokenAmnt = pfromData $ psndBuiltin # refTokenPair
      userTokenName = pfromData $ pfstBuiltin # userTokenPair 
      userTokenAmnt = pfromData $ psndBuiltin # userTokenPair 

  PPair userLabel _userTn <- pmatchC (pbreakTokenName userTokenName)
  PPair refLabel _refTn <- pmatchC (pbreakTokenName refTokenName)

  pure $
    pif 
      ( pmatch redeemer $ \case 
          PMint ->
            pand'List 
              [ userTokenAmnt #== pconstant 1
              , refTokenAmnt #== pconstant 1
              , refLabel #== label100 
              , userLabel #== label222 
              , pany @PBuiltinList 
                  # plam (\txo -> pletFields @["value", "address", "datum"] txo (\txoF -> 
                      pmatch (pfield @"credential" # txoF.address) (\case
                        PPubKeyCredential _ -> perror 
                        PScriptCredential vh ->
                          (pfield @"_0" # vh) #== metadataControlValHash) 
                            #&& pvalueOf # txoF.value # pfromData ownCS # refTokenName #== 1
                            #&& pmatch txoF.datum (\case
                                  POutputDatum dat -> (pfromPDatum @PMintMetadataDatum # (pfield @"outputDatum" # dat)) #== enforcedNFTMetadata
                                  _ -> perror 
                                  )))            
                  # txOutputs               
              , pnull # (ptail # (ptail # mintedItems))
              , pany @PBuiltinList 
                  # plam (\txo -> pletFields @["value", "address"] txo $ \txoF -> 
                        pmatch (pfield @"credential" # txoF.address) $ \case
                          PPubKeyCredential _ -> perror 
                          PScriptCredential vh -> (pfield @"_0" # vh) #== treasuryValHash 
                             #&& pvalueOf # txoF.value # padaSymbol # padaToken #>= mintingCost)
                  # txOutputs 
              , pany @PBuiltinList 
                  # plam
                    ( \txIn ->
                        let txInRef = pfield @"outRef" # txIn
                         in txInRef #== oref
                    )
                  # infoF.inputs
              ]
          PBurn -> 
            let listLengthIsTwo = pnull # (ptail # (ptail # mintedItems))
             in pand'List 
                  [ userTokenAmnt #== pconstant (-1)
                  , refTokenAmnt #== pconstant (-1)
                  , listLengthIsTwo
                  ]
      ) 
      (pconstant ())
      perror 

-- ClosedTerm (PTxOutRef :--> PData -> PScriptContext -> POpaque)
emurgoMintingPolicy :: ClosedTerm (PTxOutRef :--> PMintingPolicy)
emurgoMintingPolicy = plam $ \oref redeemer ctx -> unTermCont $ do 
  (redmr, _) <- ptryFromC @(PAsData PMintAction) redeemer 
  pure $ popaque $ emurgoMintingPolicyT # oref # pfromData redmr # ctx


-- emurgoOnchainMetadataValidator :: ClosedTerm (PAsData PCurrencySymbol :--> PValidator)
-- emurgoOnchainMetadataValidator = phoistAcyclic $ plam $ \emurgoCS _dat _redeemer context -> unTermCont $ do
--   ctx <- pletFieldsC @'["txInfo"] context 
--   txInfoF <- pletFieldsC @'["inputs", "outputs"] ctx.txInfo 

-- PValidator := PData :--> PData :--> PScriptContext :--> PUnit 
emurgoOnchainMetadataValidatorW :: ClosedTerm (PAsData PCurrencySymbol :--> PValidator)
emurgoOnchainMetadataValidatorW = phoistAcyclic $ plam $ \emurgoCS _dat _redeemer _context -> 
  pif (emurgoCS #== emurgoCS) perror perror 

(#>) :: POrd t => Term s t -> Term s t -> Term s PBool
a #> b = b #< a

infix 4 #>

(#>=) :: (POrd t) => Term s t -> Term s t -> Term s PBool
a #>= b = b #<= a

infix 4 #>=