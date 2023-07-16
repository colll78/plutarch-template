{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ParamApp where

import Plutarch
import Plutarch.Prelude
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
--import Data.Data qualified as TypeData

data DataParam = 
  DataParam {
    pkh :: PubKeyHash
  , password :: BuiltinByteString
  }
PlutusTx.makeLift ''DataParam 
PlutusTx.makeIsDataIndexed ''DataParam [('DataParam, 0)]

-- class ToRepr (a :: Type) where 
--   toSchema :: a -> String

-- getConstructors :: (a :: Type) => [String] 
-- getConstructors = 
--   let constrs :: [Constr] 
--       constrs = TypeData.dataTypeConstrs $ TypeData.dataTypeOf (undefined :: a)
--    in constrNames = map show constrs 

-- getFieldNames :: (a :: Type) => [String] 
-- getFieldNames = TypeData.constrFields . head . TypeData.dataTypeConstrs $ TypeData.dataTypeOf (undefined :: a)
--(constrFields . toConstr) ourType 

-- ourConstructors :: (a :: Type) => [Constr] 
-- ourConstructors = TypeData.dataTypeConstrs $ TypeData.dataTypeOf (undefined :: a)

-- ourTypeFields :: [[String]]
-- ourTypeFields = map TypeData.constrFields ourConstructors

-- toSchema :: (ToData a) => a -> String 
-- toSchema ourType = jsConstDef ++ (toSchemaAux ourType ourFields)
--   where 
--     ourFields :: (a :: Type) => [String]
--     ourFields = getFieldNames @a

--     constrPrec :: (ToData a) => [a] -> [String] -> String 
--     constrPrec (d:ds) (x:xs) = 
--       let constrTypes = x ++ " : " ++ (toSchemaAux d) ++ "\n" ++ (constrPrec ds xs)
--        in constrTypes 
--     constrPrec _ _ -> "" 

--     toSchemaAux :: (ToData a) => a -> [String] -> String 
--     toSchemaAux dType dFields = 
--         case dataEncodedType of 
--           Constr _i ds -> 
--             let constrStart = "Data.Object({"
--                 constrEnd = "})"
--                 constrTypes = constrPrec ds ourFields
--           Map keys values -> 
--             let mapStart = "Data.Map("
--                 mapClose = ")"
--                 keyType = 
--                   case keys of 
--                     x:_ -> (toSchemaAux x dFields)
--                     _ -> "Data.Any()" 
--                 valType = 
--                   case values of 
--                     x:_ -> (toSchemaAux x dFields)
--                     _ -> "Data.Any()"
--              in mapStart ++ keyType ++ valType ++ mapClose 
--           List ds -> 
--             let arrStart = "Data.Array("
--                 arrClose = ")"
--                 arrType = 
--                   case ds of 
--                     x:_ -> (toSchemaAux x dFields)
--                     _ -> "Data.Any()"
--              in arrStart ++ arrType ++ arrClose
--           I i -> "Data.Integer()\n"
--           B b -> "Data.Bytes()"

--     constructorName :: String 
--     constructorName = (head . words . show) ourType
    
--     jsConstDef :: String 
--     jsConstDef = "const " ++ constructorName ++ " = "

--     dataEncodedType :: Data 
--     dataEncodedType = (toData ourType)

-- const OurType = Data.Integer()

-- Data
-- Constr int [Data]          =>    Data.Object({fieldName: TYPEHERE })
-- Map Data Data              =>    Data.Map(Data.Any(), Data.Any())
-- List [Data]                =>    Data.Array(Data.Any())
-- I int                      =>    Data.Integer() 
-- B bytestring               =>    Data.Bytes()

-- const Parameter = Data.Object({
--   pubkey_hash: Data.Bytes({ minLength: 28, maxLength: 28 }),
--   password: Data.Bytes(),
-- });
-- type Parameter = Data.Static<typeof Parameter>;

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

-- PValidator := PData -> PData -> PScriptContext -> PUnit 
-- Validator := BuiltinData -> BuiltinData -> ScriptContext -> () 
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