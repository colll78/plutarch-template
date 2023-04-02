{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module EmurgoMinting (emurgoMintingPolicy) where

import Plutarch.Api.V2 
import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.DataRepr
import Plutarch.Api.V1.Value 
import Plutarch.Prelude
import Plutarch.Extra.IsData (EnumIsData (..), PlutusTypeEnumData)
import Plutarch.Extra.Value (psymbolValueOf')
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont 

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

treasuryValHash :: Term s PScriptHash 
treasuryValHash = pconstant "deadbeef"

mintingCost :: Term s PInteger 
mintingCost = 75_000_000

emurgoMintingPolicyT :: Term s (PTxOutRef :--> PMintAction :--> PScriptContext :--> PUnit)
emurgoMintingPolicyT = phoistAcyclic $ plam $ \oref redeemer ctx -> unTermCont $ do
  ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx 
  infoF <- pletFieldsC @'["mint", "outputs", "inputs"] ctxF.txInfo -- Value [(cs, [(tnA, -1), (tnB, 1)])]

  PMinting purpose <- pmatchC ctxF.purpose 
  ownCS <- pletC (pfield @"_0" # purpose)
  
  PJust mints <- pmatchC $ psymbolValueOf' # ownCS # infoF.mint
  PPair minted burned <- pmatchC mints 
  let txOutputs :: Term _ (PBuiltinList PTxOut) 
      txOutputs = infoF.outputs 

  pure $
    pif 
      ( pmatch redeemer $ \case 
          PMint -> 
            minted #== 1 
              #&& burned #== 0  
              #&& pany @PBuiltinList 
                    # plam (\txo -> pletFields @["value", "address"] txo $ \txoF -> 
                          pmatch (pfield @"credential" # txoF.address) $ \case
                            PPubKeyCredential _ -> perror 
                            PScriptCredential vh -> (pfield @"_0" # vh) #== treasuryValHash 
                               #&& pvalueOf # txoF.value # padaSymbol # padaToken #>= mintingCost)
                    # txOutputs 
              #&& pany @PBuiltinList 
                    # plam
                      ( \txIn ->
                          let txInRef = pfield @"outRef" # txIn
                           in txInRef #== oref
                      )
                    # infoF.inputs
          PBurn -> minted #== 0 #&& burned #< 0
      ) 
      (pconstant ())
      perror 

-- ClosedTerm (PTxOutRef :--> PData -> PScriptContext -> POpaque)
emurgoMintingPolicy :: ClosedTerm (PTxOutRef :--> PMintingPolicy)
emurgoMintingPolicy = plam $ \oref redeemer ctx -> unTermCont $ do 
  (redmr, _) <- ptryFromC @(PAsData PMintAction) redeemer 
  pure $ popaque $ emurgoMintingPolicyT # oref # pfromData redmr # ctx


(#>) :: POrd t => Term s t -> Term s t -> Term s PBool
a #> b = b #< a

infix 4 #>

(#>=) :: (POrd t) => Term s t -> Term s t -> Term s PBool
a #>= b = b #<= a

infix 4 #>=