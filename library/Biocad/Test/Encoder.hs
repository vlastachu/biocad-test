module Biocad.Test.Encoder where

import           Biocad.Test.Prelude
import           Biocad.Test.Data

encodeInput :: Molecule -> Value
encodeInput Molecule {..} = M [("iupacName", T iupacName), ("smiles", T smiles)]

encodeCatalyst :: (Catalyst, Accelerate) -> Value
encodeCatalyst (Catalyst {..}, Accelerate {..}) = M
  [ ("node"        , M [("iupacName", T cIupacName), ("smiles", T cSmiles)])
  , ("relationship", M [("temperature", F temperature), ("pressure", F pressure)])
  ]

encodeResult :: (Molecule, ProductFrom) -> Value
encodeResult (Molecule {..}, ProductFrom {..}) = M
  [ ("node"        , M [("iupacName", T iupacName), ("smiles", T smiles)])
  , ("relationship", M [("amount", F amount)])
  ]

