module Biocad.Test.BoltAction where

import           Biocad.Test.Prelude
import           Biocad.Test.Data
import           NeatInterpolation
import           Biocad.Test.Encoder
import           Biocad.Test.Decoder
import           Data.Map.Strict      ((!))

getMoleculeById :: MonadIO m => Int -> BoltActionT m [Record]
getMoleculeById mid =
  queryP "MATCH (n:Molecule) where ID(n) = {mid} RETURN n LIMIT 1" [("mid", I mid)]

createMolecule :: MonadIO m => Text -> Text -> BoltActionT m ()
createMolecule iupac smile = queryP_
  "CREATE (m1:Molecule { iupacName: {iupac}, smiles: {smile} })"
  [("iupac", T iupac), ("smile", T smile)]

getReactionById :: (MonadIO m, MonadFail m) => Int -> BoltActionT m (Maybe CompositeReaction)
getReactionById rid = (lift . decodeReaction) =<< queryP queryText [("rid", I rid)]
  where
    queryText = [text|
        MATCH (reaction:Reaction)
        WHERE id(reaction) = {rid}
        OPTIONAL MATCH (input)-[:REAGENT_IN]->(reaction), (catalyst)-[accelerate:ACCELERATE]->(reaction), (output)<-[product_from:PRODUCT_FROM]-(reaction)
        WHERE id(reaction) = {rid}
        RETURN reaction, collect(distinct input) as inputs, collect(distinct catalyst) as catalysts, 
        collect(distinct accelerate) as accelerates, collect(distinct output) as outputs, collect(distinct product_from) as product_froms
      |]
    decodeReaction :: (MonadIO m, MonadFail m) => [Record] -> m (Maybe CompositeReaction)
    decodeReaction (record:_) = do
      let reactionId = Just rid
      (S (Structure _ [_, _, M fieldsMap])) <- record `at` "reaction"
      reactionName <- exact $ fieldsMap ! "name"
      inputs <- decodeFmap decodeMolecule =<< record `at` "inputs"
      results' <- decodeFmap decodeMolecule =<< record `at` "outputs"
      catalysts' <- decodeFmap decodeCatalyst =<< record `at` "catalysts"
      accelerates <- decodeFmap decodeAccelerate =<< record `at` "accelerates"
      product_froms <- decodeFmap decodeProductFrom =<< record `at` "product_froms"
      let reaction = Reaction{..}
          results = zip results' product_froms
          catalysts = zip catalysts' accelerates
      pure $ Just CompositeReaction{..}
    decodeReaction _ = pure Nothing
      

createReaction :: MonadIO m => CompositeReaction -> BoltActionT m Int
createReaction CompositeReaction{..} = (lift . decodeId) =<< queryP queryText 
      [ ("reactionName", T $ reactionName reaction)
      , ("inputs", L $ encodeInput <$> inputs)
      , ("results", L $ encodeResult <$> results)
      , ("catalysts", L $ encodeCatalyst <$> catalysts)
      ] 
  where
    decodeId :: MonadIO m => [Record] -> m Int
    decodeId records = exact =<< head records `at` "reaction_id"
    queryText = [text| 
      CREATE (reaction:Reaction {name: {reactionName}})
        FOREACH (input_ IN {inputs} |
          MERGE (input:Molecule {iupacName: input_.iupacName, smiles: input_.smiles}) 
          CREATE (input)-[:REAGENT_IN]->(reaction)
        )
        FOREACH (catalyst_ IN {catalysts} |
          MERGE (catalyst:Catalyst {iupacName: catalyst_.node.iupacName, smiles: catalyst_.node.smiles})
          CREATE (catalyst)-[accelerate:ACCELERATE]->(reaction)
          SET accelerate = catalyst_.relationship
        )
        FOREACH (output_ IN {results} |
          MERGE (output:Molecule {iupacName: output_.node.iupacName, smiles: output_.node.smiles})
          CREATE (output)<-[product_from:PRODUCT_FROM]-(reaction)
          SET product_from = output_.relationship
        )
        RETURN id(reaction) as reaction_id
      |]

shortestPathBetweenMolecules :: (MonadIO m, MonadFail m) => Int -> Int -> BoltActionT m (Maybe [Int])
shortestPathBetweenMolecules startId finishId = do 
  records <- queryP queryText [("start_id", I startId), ("finish_id", I finishId)]
  case records of
    [] -> pure Nothing
    (record:_) -> do
      reactions <- record `at` "reactions"
      exact reactions
  where
    queryText = [text|
        MATCH (start:Molecule), (finish:Molecule),
        path = shortestPath((start)-[*..15]->(finish)) 
        WHERE id(start) = {start_id} AND id(finish) = {finish_id}
        RETURN [p IN nodes(path) WHERE p:Reaction | id(p)] as reactions
      |]

deleteNodeWithRelationshipsById :: (MonadIO m, MonadFail m) => Int -> BoltActionT m ()
deleteNodeWithRelationshipsById rid = queryP_ queryText [("rid", I rid)]
  where
    queryText = [text|
        MATCH (n)
        WHERE id(n) = {rid}
        OPTIONAL MATCH (n)-[r]-()
        WHERE id(n) = {rid}
        DELETE r, n
      |]

deleteNodeWithRelationshipsByPatternSmiles :: (MonadIO m, MonadFail m) => Text -> BoltActionT m ()
deleteNodeWithRelationshipsByPatternSmiles pat = queryP_ queryText [("pattern", T pat)]
  where
    queryText = [text|
        MATCH (n)
        WHERE n.smiles =~ {pattern}
        OPTIONAL MATCH (n)-[r]-()
        WHERE n.smiles =~ {pattern}
        DELETE r, n
      |]

