module Main where

import           Prelude
import           Test.HUnit
import           Database.Bolt           hiding ( pack )
import           Data.Default
import           Data.Text                      ( pack )
import           Biocad.Test.Data
import           Biocad.Test.BoltAction
import qualified OptparseApplicative.Simple.IO as OptParse
import qualified OptparseApplicative.Simple.Parser
                                               as OptParse

textShow = pack . show

sampleMolecule i = Molecule
  { moleculeId = Nothing
  , smiles     = "sample molecule " <> textShow i
  , iupacName  = "sample molecule " <> textShow i
  }

reactionFromTo from to = CompositeReaction
  { reaction  = Reaction
    { reactionId   = Nothing
    , reactionName = "sampleReaction" <> textShow from <> textShow to
    }
  , inputs    = [sampleMolecule from]
  , catalysts = [ ( Catalyst
                    { catalystId = Nothing
                    , cSmiles    = "sample catalyst 1"
                    , cIupacName = "sample catalyst 1"
                    }
                  , Accelerate {temperature = 100, pressure = 10000}
                  )
                ]
  , results   = [(sampleMolecule to, ProductFrom {amount = 1})]
  }

sampleReaction = (reactionFromTo 1 3)
  { inputs    = [sampleMolecule 1, sampleMolecule 2]
  , catalysts = [ ( Catalyst
                    { catalystId = Nothing
                    , cSmiles    = "sample catalyst 1"
                    , cIupacName = "sample catalyst 1"
                    }
                  , Accelerate {temperature = 100, pressure = 10000}
                  )
                ]
  }

emptySampleReaction =
  CompositeReaction (Reaction {reactionId = Nothing, reactionName = "sampleReaction"}) [] [] []

removeIdentifiers CompositeReaction {..} = CompositeReaction
  { reaction  = reaction { reactionId = Nothing }
  , inputs    = sort $ removeMoleculeId <$> inputs
  , catalysts = sort $ first removeCatalystId <$> catalysts
  , results   = sort $ first removeMoleculeId <$> results
  }
 where
  removeMoleculeId molecule = molecule { moleculeId = Nothing }
  removeCatalystId catalyst = catalyst { catalystId = Nothing }

testReactionOperations unsavedReaction config = TestCase $ do
  pipe                   <- connect config
  createdId              <- run pipe $ createReaction unsavedReaction
  (Just createdReaction) <- run pipe $ getReactionById createdId
  assertEqual "reaction from db should be same as sample, except ids"
              unsavedReaction
              (removeIdentifiers createdReaction)
  run pipe $ deleteNodeWithRelationshipsById createdId
  run pipe $ deleteNodeWithRelationshipsByPatternSmiles "sample.*"
  reactionAfterDelete <- run pipe $ getReactionById createdId
  assertEqual "reaction should be deleted" Nothing reactionAfterDelete

testShortestPath config = TestCase $ do
  pipe <- connect config
  let sampleReactions =
        uncurry reactionFromTo
          <$> ([(1, 2), (2, 3), (3, 4), (3, 5), (1, 6), (6, 7), (5, 7), (4, 7)] :: [(Int, Int)])
  ids       <- mapM (run pipe . createReaction) sampleReactions
  reactions <- mapM (run pipe . getReactionById) ids
  assertBool "all reactions should be created in db" $ all isJust reactions
  let
    start          = fromJust $ moleculeId $ head $ inputs $ fromJust $ head reactions
    finish         = fromJust $ moleculeId $ fst $ head $ results $ fromJust $ last reactions
    expectablePath = Just $ fromJust . reactionId . reaction . fromJust . (reactions !!) <$> [4, 5]
  path <- run pipe $ shortestPathBetweenMolecules start finish
  assertEqual "not the shortest path" expectablePath path
  mapM_ (run pipe . deleteNodeWithRelationshipsById) ids
  run pipe $ deleteNodeWithRelationshipsByPatternSmiles "sample.*"

main = do
  (username, password) <-
    OptParse.parser ""
    $   (,)
    <$> OptParse.lenientArgument "username" Nothing (Just "A username for neo4j") Nothing
    <*> OptParse.lenientArgument "password" Nothing (Just "A password for neo4j") Nothing
  let config = def { user = username, password = password, version = 2 }
  runTestTT $ TestList
    [ TestLabel "test full reaction" $ testReactionOperations sampleReaction config
    , TestLabel "test empty reaction" $ testReactionOperations emptySampleReaction config
    , TestLabel "test shortest path" $ testShortestPath config
    ]

