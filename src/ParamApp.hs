{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module ParamApp where

import Plutarch (Config)
import Plutarch.Api.V1 (
  PCredential (PPubKeyCredential, PScriptCredential),
 )
import Plutarch.Api.V2 
import Plutarch.DataRepr (
  DerivePConstantViaData (..),
  PDataFields,
 )
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
  ptryFromC
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  MintingPolicy,
  TxOutRef,
 )
import PlutusTx qualified

data PDataParam (s :: S)
  = PDataParam
      ( Term
          s
          ( PDataRecord
              '[ "pkh" ':= PPubKeyHash
               , "password" ':= PByteString
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PDataParam where
  type DPTStrat _ = PlutusTypeData

data PTestRedeemer (s :: S)
  = PTestPKH (Term s (PDataRecord '[]))
  | PTestPassword (Term s (PDataRecord '["_0" ':= PByteString]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PShow)

instance DerivePlutusType PTestRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PTestRedeemer
instance PTryFrom PData (PAsData PTestRedeemer)

pvalidateTestTyped :: Term s (PDataParam :--> PTestRedeemer :--> PScriptContext :--> PUnit)
pvalidateTestTyped = phoistAcyclic $ plam $ \dparam redeemer ctx -> unTermCont $ do 
  ctxF <- pletFieldsC @'["txInfo"] ctx 
  infoF <- pletFieldsC @'["signatories"] ctxF.txInfo 
  dparamF <- pletFieldsC @'["pkh", "password"] dparam 
  pure $
    pif
      ( pmatch
          redeemer
          ( \case
            PTestPKH _ -> pelem # dparamF.pkh # infoF.signatories
            PTestPassword pass -> (pfield @"_0" # pass) #== dparamF.password  
          )
      )
      (pconstant ())
      perror

pvalidateTestW :: Term s (PDataParam :--> PValidator)
pvalidateTestW = phoistAcyclic $ plam $ \dparam _dat redeemer ctx -> unTermCont $ do 
  (redm, _) <- ptryFromC @PTestRedeemer redeemer
  pure $ popaque $ pvalidateTestTyped # dparam # redm # ctx
-- type DatumParam {
--   pubkey_hash: Hash<Blake2b_224, VerificationKey>,
--   password: ByteArray,
-- }

-- type UnlockRedeemer {
--   PubKey
--   Password { password: ByteArray }
-- }

-- validator(param: DatumParam) {
--   fn unlock(redeemer: UnlockRedeemer, context: ScriptContext) {
--     when redeemer is {
--       PubKey ->
--         list.has(context.transaction.extra_signatories, param.pubkey_hash)
--       Password { password } ->
--         password == param.password
--     }
--   }
-- }