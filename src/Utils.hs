{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Utils where 

import Plutarch.Api.V2
import Plutarch.Prelude
import Plutarch.Bool
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont 
import "liqwid-plutarch-extra" Plutarch.Extra.List (plookupAssoc)

pexpectJust :: Term s r -> Term s (PMaybe a) -> TermCont @r s (Term s a)
pexpectJust escape ma = tcont $ \f -> pmatch ma $ \case
  PJust v -> f v 
  PNothing -> escape 

psymbolValueOfHelper ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term
    s
    ( (PInteger :--> PBool)
        :--> PCurrencySymbol
        :--> ( PValue keys amounts
                :--> PInteger
             )
    )
psymbolValueOfHelper =
  phoistAcyclic $
    plam $ \cond sym value'' -> unTermCont $ do
      PValue value' <- pmatchC value''
      PMap value <- pmatchC value'
      m' <-
        pexpectJust
          0
          ( plookupAssoc
              # pfstBuiltin
              # psndBuiltin
              # pdata sym
              # value
          )
      PMap m <- pmatchC (pfromData m')
      pure $
        pfoldr
          # plam
            ( \x v ->
                plet (pfromData $ psndBuiltin # x) $ \q ->
                  pif
                    (cond # q)
                    (q + v)
                    v
            )
          # 0
          # m

-- | @since 1.0.0
ppositiveSymbolValueOf ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PCurrencySymbol :--> (PValue keys amounts :--> PInteger))
ppositiveSymbolValueOf = phoistAcyclic $ psymbolValueOfHelper #$ plam (0 #<)

-- | @since 1.0.0
pnegativeSymbolValueOf ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PCurrencySymbol :--> (PValue keys amounts :--> PInteger))
pnegativeSymbolValueOf = phoistAcyclic $ psymbolValueOfHelper #$ plam (#< 0)

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