import System.Environment
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.Tree.NTree.TypeDefs
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
  titles <- runX $ getMovieTitle doc
  let movie = Movie { title = (titles !! 0) }
  actors <- parseActors doc
  let credentials = (BS.pack "neo4j", BS.pack password)
  {- persist to DB -}
  withAuthConnection (BS.pack "localhost") 7474 credentials $ do
    B.runBatch $ do
      spr <- addMovieNode movie
      addLabel (T.pack "Movie") spr
      actorNodes <- mapM addActorNode actors
      mapM_ (addLabel (T.pack "Actor")) actorNodes
      mapM_ (createMovieToActorRelationship spr) actorNodes
  print "Success!"

{- HTML request -}

getHTML :: String -> IO String
getHTML url = simpleHTTP (getRequest url) >>= getResponseBody

{- HTML parsing -}

data Actor = Actor { name :: String, url :: String }
data Movie = Movie { title :: String }

getMovieTitle :: IOSArrow XmlTree (NTree XNode) -> IOSLA (XIOState ()) XmlTree String
getMovieTitle tree = tree //> hasAttrValue "class" (== "header") /> hasAttrValue "itemprop" (== "name") /> getText

parseActors :: IOSArrow XmlTree (NTree XNode) -> IO [Actor]
parseActors tree = do
  actorProps <- runX $ tree //> hasAttrValue "itemprop" (== "actor") >>> (parseActorName &&& parseActorUrl)
  return $ map (\(x,y) -> Actor { name = x, url = y }) actorProps

parseActorName :: IOSLA (XIOState ()) XmlTree String
parseActorName = deep $ hasAttrValue "itemprop" (== "name") /> getText

parseActorUrl :: IOSLA (XIOState ()) XmlTree String
parseActorUrl = deep $ hasAttrValue "itemprop" (== "url") >>> getAttrValue "href"

{- DB Interaction -}

addMovieNode :: Movie -> B.Batch (B.BatchFuture Node)
addMovieNode movie = B.createNode $ M.fromList [ (T.pack "title") |: (T.pack $ title movie)
                                               ]

addActorNode :: Actor -> B.Batch (B.BatchFuture Node)
addActorNode actor = B.createNode $ M.fromList [ (T.pack "name") |: (T.pack $ name actor)
                                               , (T.pack "url")   |: (T.pack $ url actor) 
                                               ]

createMovieToActorRelationship :: B.BatchFuture Node -> B.BatchFuture Node -> B.Batch (B.BatchFuture Relationship)
createMovieToActorRelationship movieNode actorNode = B.createRelationship (T.pack "ACTED_IN") M.empty actorNode movieNode

addLabel :: Label -> B.BatchFuture Node -> B.Batch (B.BatchFuture ())
addLabel label node = B.addLabels [label] node


