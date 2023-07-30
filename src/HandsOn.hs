{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module HandsOn where

import Plutarch.Monadic qualified as P
import Plutarch
import Plutarch.Prelude
import Plutarch.Extra.IsData (EnumIsData (..), PlutusTypeEnumData)
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
import PlutusLedgerApi.V2 (
  PubKeyHash,
  BuiltinByteString
 )
import PlutusTx qualified

data PDatumExample (s :: S)
  = PDatumExample
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

instance DerivePlutusType PDatumExample where
  type DPTStrat _ = PlutusTypeData

data PScottExample (s :: S) = PScottExample
  { member :: Term s PByteString
  , committed :: Term s PInteger
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PScottExample where
  type DPTStrat _ = PlutusTypeScott

data PMintAction (s :: S) = PMint | PBurn
 deriving stock (Generic, Enum, Bounded)
 deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PMintAction where
  type DPTStrat _ = PlutusTypeEnumData 

instance PTryFrom PData (PAsData PMintAction)

projectKey :: Term s (PAsData PPubKeyHash)
projectKey = pdata $ pconstant "deadbeef"

data PMyWonderfullDatum (s :: S) = 
  PMWDn (Term s (PDataRecord '["_0" ':= PInteger]))
  | PMWDb (Term s (PDataRecord '["_0" ':= PInteger])) 
  | PMWDan (Term s (PDataRecord '["_0" ':= PByteString])) 
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PMyWonderfullDatum where 
  type DPTStrat _ = PlutusTypeData

data PMyWonderfullRedeemer (s :: S) = PMyWonderfullRedeemer (Term s (PDataRecord '["_0" ':= PInteger]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType PMyWonderfullRedeemer where 
  type DPTStrat _ = PlutusTypeData

handsOn :: Term s (PData :--> PData :--> PScriptContext :--> POpaque) 
handsOn = phoistAcyclic $ plam $ \_dat _red ctx -> P.do 
  --let txInfo = (pfield @"txInfo" # ctx)
  ctxF <- pletFields @'["txInfo", "purpose"] ctx 
  infoF <- pletFields @'["signatories", "validRange"] ctxF.txInfo
  
  pif (pelem # projectKey # infoF.signatories)
    (popaque $ pconstant ())
    perror 

-- data Data = Constr Integer [Data]	 
--           | Map [(Data, Data)]	 
--           | List [Data]	 
--           | I Integer	 
--           | B ByteString

