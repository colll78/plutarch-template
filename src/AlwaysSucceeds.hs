module AlwaysSucceeds (validator) where

import Plutarch.Api.V2 (
  PValidator,
 )
import Plutarch.Prelude

validator :: ClosedTerm PValidator
validator = plam $ \_ _ _ -> popaque $ pconstant True
