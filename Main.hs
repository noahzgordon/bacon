{-# LANGUAGE OverloadedStrings #-}

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
  let password = BS.pack $ args !! 1
  html <- getHTML url
  {- find names -}
  let doc = readString [withParseHTML yes, withWarnings no] html
  movie <- parseMovieFromMoviePage url doc
  actors <- parseActorsFromMoviePage doc
  let credentials = ("neo4j", password)
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

data Actor = Actor { name :: T.Text, actorUrl :: T.Text }
data Movie = Movie { title :: T.Text, movieUrl :: T.Text }

parseMovieFromMoviePage :: String -> IOSArrow XmlTree (NTree XNode) -> IO Movie
parseMovieFromMoviePage url tree = do
  title <- fmap head $ runX $ getMovieTitle tree
  return Movie { title = (T.pack title), movieUrl = (T.pack url) }

getMovieTitle :: IOSArrow XmlTree (NTree XNode) -> IOSLA (XIOState ()) XmlTree String
getMovieTitle tree = tree //> hasAttrValue "class" (== "header") /> hasAttrValue "itemprop" (== "name") /> getText

parseActorsFromMoviePage :: IOSArrow XmlTree (NTree XNode) -> IO [Actor]
parseActorsFromMoviePage tree = do
  actorProps <- runX $ tree //> hasAttrValue "itemprop" (== "actor") >>> (parseActorName &&& parseActorUrl)
  return $ map (\(name,url) -> Actor { name = (T.pack name), actorUrl = (T.pack url) }) actorProps

parseActorName :: IOSLA (XIOState ()) XmlTree String
parseActorName = deep $ hasAttrValue "itemprop" (== "name") /> getText

parseActorUrl :: IOSLA (XIOState ()) XmlTree String
parseActorUrl = deep $ hasAttrValue "itemprop" (== "url") >>> getAttrValue "href"

{- DB Interaction -}

addMovieNode :: Movie -> B.Batch (B.BatchFuture Node)
addMovieNode movie = B.createNode $ M.fromList [ "title" |: title movie
                                               , "url"   |: movieUrl movie
                                               ]
addActorNode :: Actor -> B.Batch (B.BatchFuture Node)
addActorNode actor = B.createNode $ M.fromList [ "name"  |: name actor
                                               , "url"   |: actorUrl actor
                                               ]

createMovieToActorRelationship :: B.BatchFuture Node -> B.BatchFuture Node -> B.Batch (B.BatchFuture Relationship)
createMovieToActorRelationship movieNode actorNode = B.createRelationship (T.pack "ACTED_IN") M.empty actorNode movieNode

addLabel :: Label -> B.BatchFuture Node -> B.Batch (B.BatchFuture ())
addLabel label node = B.addLabels [label] node


