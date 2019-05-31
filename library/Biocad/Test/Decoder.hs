module Biocad.Test.Decoder(decodeFmap, decodeMolecule, decodeCatalyst, decodeAccelerate, decodeProductFrom) where

import           Biocad.Test.Prelude
import           Biocad.Test.Data
import           Data.Map.Strict      ((!))

decodeFmap :: (MonadIO m, MonadFail m) => (Value -> m b) -> Value -> m [b]
decodeFmap f (L l) = sequence $ f <$> l
decodeFmap f _     = fail "not a list"

extractNodeProps :: (Monad m, RecordValue a) => Node -> [Text] -> m [a]
extractNodeProps (Node _ _ props) propsNames = sequence $ exact . (props !) <$> propsNames

extractRelationshipProps :: (Monad m, RecordValue a) => Relationship -> [Text] -> m [a]
extractRelationshipProps (Relationship _ _ _ _ props) propsNames = sequence $ exact . (props !) <$> propsNames

decodeMolecule :: (MonadIO m, MonadFail m) => Value -> m Molecule
decodeMolecule nodeValue = do
  node <- exact nodeValue
  let moleculeId = Just $ nodeIdentity node
  [iupacName, smiles] <- extractNodeProps node ["iupacName", "smiles"]
  pure Molecule {..}

decodeCatalyst :: (MonadIO m, MonadFail m) => Value -> m Catalyst
decodeCatalyst nodeValue = do
  node <- exact nodeValue
  let catalystId = Just $ nodeIdentity node
  [cIupacName, cSmiles] <- extractNodeProps node ["iupacName", "smiles"]
  pure Catalyst {..}

decodeAccelerate :: (MonadIO m, MonadFail m) => Value -> m Accelerate
decodeAccelerate relationshipValue = do
  relationship <- exact relationshipValue
  [temperature, pressure] <- extractRelationshipProps relationship ["temperature", "pressure"]
  pure Accelerate {..}

decodeProductFrom :: (MonadIO m, MonadFail m) => Value -> m ProductFrom
decodeProductFrom relationshipValue = do
  relationship <- exact relationshipValue
  amount <- exact $ relProps relationship ! "amount"
  pure ProductFrom {..}
