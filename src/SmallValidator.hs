{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module SmallValidator where

import Plutarch
import Plutarch.Prelude
import Data.Text qualified as T
import Plutarch.Monadic qualified as P 
import Plutarch.Api.V1 (
  PCredential (PPubKeyCredential, PScriptCredential),
 )
import Plutarch.Api.V2 
import Plutarch.DataRepr
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
  ptryFromC
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusTx qualified

--data OurDatum = OurDatum {password :: BuiltinByteString}

ptryOwnInput :: Term s (PBuiltinList PTxInInfo :--> PTxOutRef :--> PTxOut)
ptryOwnInput = phoistAcyclic $
  plam $ \inputs ownRef ->
    precList (\self x xs -> pletFields @'["outRef", "resolved"] x $ \txInFields -> pif (ownRef #== txInFields.outRef) txInFields.resolved (self # xs)) (const perror) # inputs

passert ::
  forall (s :: S) (a :: PType).
  T.Text -> -- long trace
  Term s PBool ->
  Term s a ->
  Term s a
passert longErrorMsg b inp = pif b inp $ ptraceError (pconstant longErrorMsg)


data POurDatum (s :: S) = POurDatum (Term s (PDataRecord '[ "password" ':= PByteString ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType POurDatum where 
    type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData POurDatum 

data POurRedeemer (s :: S) = 
  PPassword (Term s (PDataRecord '[ "password" ':= PByteString ]))
  | PGuess (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType POurRedeemer where 
    type DPTStrat _ = PlutusTypeData

instance PTryFrom PData POurRedeemer  


-- validateSmallChecks :: OurDatum -> OurRedeemer -> ScriptContext -> () 
pvalidateSmallChecks :: Term s (POurDatum :--> POurRedeemer :--> PScriptContext :--> PUnit)
pvalidateSmallChecks = phoistAcyclic $ plam $ \_datum redeemer _ctx -> unTermCont $ do 
    pure $ 
      pif
        ( pmatch
            redeemer 
            ( \case 
              PPassword _ -> pconstant True 
              PGuess _ -> pconstant True 
            ) 
        )
        (pconstant ())
        perror 

ptryOwnOutput :: (PIsListLike list PTxOut) => Term s (list PTxOut :--> PAddress :--> PTxOut)
ptryOwnOutput = phoistAcyclic $
  plam $ \outs target ->
    ( pfix #$ plam $ \self xs ->
        pelimList
          ( \txo txos ->
              pif (target #== (pfield @"address" # txo)) txo (self # txos)
          )
          perror
          xs
    )
      # outs

-- type PValidator = PData :--> PData :--> PScriptContext :--> POpaque
pvalidateSmallChecksW :: Term s PValidator 
pvalidateSmallChecksW = phoistAcyclic $ plam $ \datum redeemer ctx ->
    let ourDatum :: Term _ POurDatum 
        ourDatum = punsafeCoerce datum 
        ourRedeemer :: Term _ POurRedeemer 
        ourRedeemer = punsafeCoerce redeemer
     in popaque $ pvalidateSmallChecks # ourDatum # ourRedeemer # ctx 


data PFunDatum (s :: S) = PFunDatum 
  (Term s 
    (PDataRecord 
      '["password" ':= PByteString
       , "index" ':= PInteger 
       ]
    )
  )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PFunDatum where 
    type DPTStrat _ = PlutusTypeData 

fooValidator :: Term s (PFunDatum :--> PData :--> PScriptContext :--> PUnit)
fooValidator = phoistAcyclic $ plam $ \dat _ ctx -> P.do 
  datF <- pletFields @'["password", "index"] dat 
  ctxF <- pletFields @'["purpose", "txInfo"] ctx
  infoF <- pletFields @'["inputs", "mint", "outputs"] ctxF.txInfo 

  -- PSpending ((pfield @"_0" #) -> ownRef) <- pmatch ctxF.purpose 
  PSpending ownRef' <- pmatch ctxF.purpose 
  ownRef <- plet (pfield @"_0" # ownRef') 

  let ownInput = ptryOwnInput # infoF.inputs # ownRef
  ownInputF <- pletFields @'["value", "address"] ownInput 
  
  ownAddress <- plet ownInputF.address
  
  let ownOutput = ptryOwnOutput # infoF.outputs # ownAddress 
  ownOutputF <- pletFields @'["value", "address"] ownOutput 

 -- (passert "value is preserved" $ ownInputF.value #== ownOutputF.value)

  (pconstant ()) 

data PSomething (s :: S) = 
  PA (Term s (PDataRecord '[ "password" ':= PByteString ]))
  | PB (Term s (PDataRecord '[]))
  | PC (Term s (PDataRecord '[]))
  | PD (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PSomething where 
    type DPTStrat _ = PlutusTypeData

pcheckSomething :: Term s PSomething -> Term s PBool 
pcheckSomething something = 
  pmatch something $ \case 
    PA _ -> pconstant False 
    PB _ -> pconstant True
    PC _ -> perror 
    PD _ -> pconstant True 