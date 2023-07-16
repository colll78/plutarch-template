{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module DAOValidator (emurgoDAOValidatorW, pvalidateDaoStateMintW) where

import Plutarch.Api.V2 
import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.DataRepr
import Plutarch.Api.V1.Value 
import Plutarch.Bool
import Plutarch.Prelude
import Plutarch.Extra.ScriptContext (ptryFromInlineDatum, pfromPDatum)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont 
import PlutusLedgerApi.V1.Value (AssetClass(..))

-- Smart Handle $adaToMin 
-- user sends 50 ADA to $adaToMin
-- 50 ADA is swapped for Min Token and user receives the result
-- Expand given list of conditions with pand' 
-- evalutates arguments strictly
pand'List :: [Term s PBool] -> Term s PBool
pand'List = foldr1 (\res x -> pand' # res # x)

pcond :: [(Term s PBool, Term s a)] ->
  Term s a -> 
  Term s a 
pcond [] def = def 
pcond ((cond, x) : conds) def = pif cond x (pcond conds def)

(#>) :: PPartialOrd t => Term s t -> Term s t -> Term s PBool
a #> b = b #< a
infix 4 #>

(#>=) :: (PPartialOrd t) => Term s t -> Term s t -> Term s PBool
a #>= b = b #<= a
infix 4 #>=


data PAssetClass (s :: S) = PAssetClass (Term s (PDataRecord '[ "cs" ':= PCurrencySymbol, "tn" ':= PTokenName]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PAssetClass where 
    type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData PAssetClass 


instance PUnsafeLiftDecl PAssetClass where
  type PLifted PAssetClass = AssetClass

deriving via
  (DerivePConstantViaData AssetClass PAssetClass)
  instance
    (PConstantDecl AssetClass)
     
data PSmartHandleDatum (s :: S) = PSmartHandleDatum (Term s (PDataRecord '[ "owner" ':= PAddress ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PSmartHandleDatum where 
    type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData PSmartHandleDatum 

data PSmartHandleRedeemer (s :: S) = 
  PSwap (Term s (PDataRecord '["ownIndex" ':= PInteger, "routerIndex" ':= PInteger]))
  | PReclaim (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PSmartHandleRedeemer where 
    type DPTStrat _ = PlutusTypeData

instance PTryFrom PData POurRedeemer  

data POrderType (s :: S) = POrderType
    ( Term
          s
          ( PDataRecord
              '[ "desiredAsset" ':= PAssetClass
               , "minReceive" ':= PInteger 
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType POrderType where 
    type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData POrderType 

data PMinswapRequestDatum (s :: S) = PMinswapRequestDatum
    ( Term
          s
          ( PDataRecord
              '[ "sender" ':= PAddress,
                 "receiver" ':= PAddress,
                 "receiverDatumHash" ':= PMaybeData PDatumHash,
                 "step" ':= POrderType,
                 "batcherFee" ':= PInteger,
                 "outputAda" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PMinswapRequestDatum where 
    type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData PMinswapRequestDatum 

data PSmartMetadataDatum ( s :: S ) =
  PSmartMetadataDatum (Term s (PDataRecord '["metadata" ':= PData, "version" ':= PInteger, "extra" ':= PSmartRouter]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType PMintMetadataDatum where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PMintMetadataDatum 

adaToMinTN :: Term s PTokenName
adaToMinTN = 
  let tn :: TokenName 
      tn = "$adaToMin"
   in pconstant tn 

minCS :: Term s PCurrencySymbol
minCS = 
  let cs :: CurrencySymbol
      cs = "29d222ce763455e3d7a09a665ce554f00ac89d2e99a1a83d267170c6"
   in pconstant cs 

minTN :: Term s PTokenName 
minTN = 
  let tn :: TokenName 
      tn = "MIN"
   in pconstant tn 

swapPkh :: Term s PPubKeyHash
swapPkh = 
  let orderCred :: PubKeyHash 
      orderCred = "a65ca58a4e9c755fa830173d2a5caed458ac0c73f97db7faae2e7e3b"
   in pconstant orderCred

swapAddress :: Term s PAddress 
swapAddress = 
  let orderCred = "a65ca58a4e9c755fa830173d2a5caed458ac0c73f97db7faae2e7e3b"
      orderStakeCred = "52563c5410bff6a0d43ccebb7c37e1f69f5eb260552521adff33b9c2"
      orderAddr = Address (ScriptCredential orderCred) (Just (StakingCredential orderStakeCred))
   in pconstant orderAddr 

-- data OrderDatum = OrderDatum
--   { odSender :: Address,                     => Owner 
--     odReceiver :: Address,                   => Owner
--     odReceiverDatumHash :: Maybe DatumHash,  => Constr 1 []
--     odStep :: OrderStep,                     => Constr 0 [Constr 0 [policyId, tokenName], minAmount] 
--     odBatcherFee :: Integer,                 => 2_000_000
--     odOutputADA :: Integer                   => 2_000_000
--   }
data PSmartRouter (s :: S) = PSmartRouter
    ( Term
          s
          ( PDataRecord
              -- 2 ADA Fee paid for the service of off-chain Laminar batcher to process transactions.
              '[ "batcherFee" ':= PInteger
              -- 2 ADA This amount of ADA will be held as minimum UTxO ADA and will be returned when 
              -- your order is processed or cancelled. 
               , "deposit" ':= PInteger 
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PSmartRouter where 
    type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData PSmartRouter 


data PSmartConfig (s :: S) = PSmartConfig
    ( Term
          s
          ( PDataRecord
              '[ "cs" ':= PCurrencySymbol,
               -- "addr1zxn9efv2f6w82hagxqtn62ju4m293tqvw0uhmdl64ch8uw6j2c79gy9l76sdg0xwhd7r0c0kna0tycz4y5s6mlenh8pq6s3z70"
                 "swapScript" ':= PAddress 
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PSmartConfig where 
    type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData PSmartConfig 

ptryOwnInput :: (PIsListLike list PTxInInfo) => Term s (list PTxInInfo :--> PTxOutRef :--> PTxOut)
ptryOwnInput = phoistAcyclic $
  plam $ \inputs ownRef ->
    precList (\self x xs -> 
      pletFields @'["outRef", "resolved"] x $ \txInFields -> 
        pif (ownRef #== txInFields.outRef) txInFields.resolved (self # xs)
      ) 
      (const perror)
      # inputs

psmartHandleValidator :: Term s (PSmartConfig :--> PSmartHandleDatum :--> PSmartHandleRedeemer :--> PScriptContext :--> PUnit)
psmartHandleValidator = phoistAcyclic $ plam $ \smartConfig dat red ctx -> pmatch red $ \case 
  PSwap r -> 
    pletFields @'["ownIndex", "routerIndex"] red $ \redF ->
      pswapRouter # smartConfig # dat # redF.ownIndex # redF.routerIndex # ctx 
  PReclaim _ -> 
    pelem # (pfield @"owner" # dat) # (pfield @"signatories" # (pfield @"txInfo" # ctx))

pswapRouter :: Term s (PSmartConfig :--> PSmartHandleDatum :--> PInteger :--> PInteger :--> PScriptContext :--> PUnit)
pswapRouter = phoistAcyclic $ plam $ \smartConfig dat ownIndex routerIndex ctx -> unTermCont $ do 
  configF <- pletFieldsC @'["cs", "swapScript"] smartConfig 
  oldDatumF <- pletFieldsC @'["owner"] dat 
  ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
  txInfo <- pletFieldsC @'["inputs", "outputs", "signatories"] ctxF.txInfo
  PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose
  indexedInput <- pletFieldsC @'["outRef", "resolved"] (pelemAt @PBuiltinList # ownIndex # txInfo.inputs)
  
  ownInputF <- pletFieldsC @'["value", "address"] indexedInput.resolved
  ownOutputF <- pletFieldsC @'["datum", "value", "address"] (pelemAt @PBuiltinList # ownIndex # txInfo.outputs)
  
  let outDatum = pfromPDatum @PMinswapRequestDatum # (ptryFromInlineDatum # ownOutputF.datum)
  outDatumF <- pletFieldsC @'["sender", "receiver", "receiverDatumHash", "step", "batcherFee", "outputAda"] outDatum 
  orderStepF <- pletFieldsC @'["desiredAsset", "minReceive"] outDatumF.step
  desiredAssetF <- pletFieldsC @'["cs", "tn"] orderStepF.desiredAsset

  pure $
    pif 
      ( pand'List 
          [ ownRef #== indexedInput.outRef 
          , outDatumF.sender #== oldDatumF.owner
          , outDatumF.receiver #== oldDatumF.owner 
          , pmatch outDatumF.receiverDatumHash $ \case 
              PDJust _ -> pconstant False 
              PDNothing _ -> pconstant True 
          , desiredAssetF.cs #== minCS
          , desiredAssetF.tn #== minTN 
          , pfromData outputDatumF.batcherFee #== pconstant 2_000_000
          , pfromData outputDatumF.outputAda #== pconstant 2_000_000
          , ownOutputF.address #== swapAddress 
          ]
      )
      (pconstant ())
      perror 