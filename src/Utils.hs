module Utils where 

import Plutarch.Api.V2
import Plutarch.Prelude

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

