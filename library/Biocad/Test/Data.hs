module Biocad.Test.Data where

import           Biocad.Test.Prelude

data Molecule = Molecule
  { moleculeId :: Maybe Int
  , smiles :: Text
  , iupacName :: Text
  } deriving (Eq, Show, Ord)

data Reaction = Reaction
  { reactionId :: Maybe Int
  , reactionName :: Text
  } deriving (Eq, Show)

data Catalyst = Catalyst
  { catalystId :: Maybe Int
  , cSmiles :: Text
  , cIupacName :: Text
  } deriving (Eq, Show, Ord)

data ProductFrom = ProductFrom
  { amount :: Double
  } deriving (Eq, Show, Ord)

data Accelerate = Accelerate
  { temperature :: Double
  , pressure :: Double
  } deriving (Eq, Show, Ord)

data CompositeReaction = CompositeReaction
  { reaction :: Reaction
  , inputs :: [Molecule]
  , catalysts :: [(Catalyst, Accelerate)]
  , results :: [(Molecule, ProductFrom)]
  } deriving (Eq, Show)
