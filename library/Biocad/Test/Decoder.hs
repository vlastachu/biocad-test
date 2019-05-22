module Biocad.Test.Decoder where

import           Biocad.Test.Prelude
import           Biocad.Test.Data
import           Data.Map.Strict      ((!))

decodeFmap :: (MonadIO m, MonadFail m) => (Value -> m b) -> Value -> m [b]
decodeFmap f (L l) = sequence $ f <$> l
decodeFmap f _     = fail "not a list"

decodeMolecule :: (MonadIO m, MonadFail m) => Value -> m Molecule
decodeMolecule (S (Structure _ [I moleculeId', _, M fieldsMap])) = do
  let moleculeId = Just moleculeId'
  iupacName <- exact $ fieldsMap ! "iupacName"
  smiles    <- exact $ fieldsMap ! "smiles"
  pure Molecule {..}
decodeMolecule _ = fail "not a structure"

decodeCatalyst :: (MonadIO m, MonadFail m) => Value -> m Catalyst
decodeCatalyst (S (Structure _ [I catalystId', _, M fieldsMap])) = do
  let catalystId = Just catalystId'
  cIupacName <- exact $ fieldsMap ! "iupacName"
  cSmiles    <- exact $ fieldsMap ! "smiles"
  pure Catalyst {..}
decodeCatalyst _ = fail "not a structure"

decodeAccelerate :: (MonadIO m, MonadFail m) => Value -> m Accelerate
decodeAccelerate (S (Structure _ [_, _, _, _, M fieldsMap])) = do
  temperature <- exact $ fieldsMap ! "temperature"
  pressure    <- exact $ fieldsMap ! "pressure"
  pure Accelerate {..}
decodeAccelerate _ = fail "not a structure"

decodeProductFrom :: (MonadIO m, MonadFail m) => Value -> m ProductFrom
decodeProductFrom (S (Structure _ [_, _, _, _, M fieldsMap])) = do
  amount <- exact $ fieldsMap ! "amount"
  pure ProductFrom {..}
decodeProductFrom _ = fail "not a structure"
