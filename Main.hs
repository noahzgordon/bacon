import System.Environment
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Network.HTTP
import Database.Neo4j
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import qualified Database.Neo4j.Batch as B

main :: IO ()
main = do
  {- get HTML -}
  args <- getArgs
  let url = args !! 0
  let password = args !! 1
  html <- getHTML url
  {- find names -}
  let doc = readString [withParseHTML yes, withWarnings no] html
  actors <- runX $ getActorNames doc
  let credentials = (BS.pack "neo4j", BS.pack password)
  withAuthConnection (BS.pack "localhost") 7474 credentials $ do
    B.runBatch $ do
      let spr = addMovieNode "Saving Private Ryan"
      addLabel (T.pack "Movie") spr
      let actorNodes = map addActorNode actors
      mapM_ (addLabel (T.pack "Actor")) actorNodes
      mapM_ (createMovieToActorRelationship spr) actorNodes
  print actors

addMovieNode :: String -> B.Batch (B.BatchFuture Node)
addMovieNode title = B.createNode $ M.fromList [ (T.pack "title") |: (T.pack title) ]

addActorNode :: String -> B.Batch (B.BatchFuture Node)
addActorNode name = B.createNode $ M.fromList [ (T.pack "name") |: (T.pack name) ]

createMovieToActorRelationship :: B.Batch (B.BatchFuture Node) -> B.Batch (B.BatchFuture Node) -> B.Batch (B.BatchFuture Relationship)
createMovieToActorRelationship movieNode actorNode = do
  movie <- movieNode
  actor <- actorNode
  B.createRelationship (T.pack "ACTED_IN") M.empty actor movie

addLabel :: Label -> B.Batch (B.BatchFuture Node) -> B.Batch (B.BatchFuture ())
addLabel label neoNode = do
  node <- neoNode
  B.addLabels [label] node

getActorNames tree = (getActors tree) //> hasAttrValue "itemprop" (== "name") /> getText

getActors tree = tree //> hasAttrValue "itemprop" (== "actor")

{- doc >>> deep (hasAttrValue "class" (== "itemprop")) >>> (getElemName &&& getAttrValue "itemprop") -}

getHTML :: String -> IO String
getHTML url = simpleHTTP (getRequest url) >>= getResponseBody


