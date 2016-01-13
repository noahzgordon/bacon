{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs
import Network.HTTP
import Database.Neo4j
import System.IO (hFlush, stdout, stdin, putChar, hGetEcho, hSetEcho)
import Control.Exception (bracket_)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import qualified Database.Neo4j.Batch as B

type Url = String

data Actor = Actor { name :: T.Text, actorUrl :: T.Text }
data Movie = Movie { title :: T.Text, movieUrl :: T.Text }

main :: IO ()
main = do
  {- get HTML -}
  putStrLn "In order to use this program, you must run a Neo4j server on localhost:7474."
  putStrLn "If you are not already doing so, start it up now.\n"
  putStrLn "What is your Neo4j server username?"
  username <- getLine
  putStrLn "What is your Neo4j server password?"
  password <- getPassword
  putStrLn "Please provide an IMDB movie url from which to start the Baconator."
  url <- getLine
  processMoviePage url (BS.pack username, BS.pack password)

processMoviePage :: Url -> Credentials -> IO ()
processMoviePage url credentials = do
  html <- getHTML url
  {- scrape page and build data -}
  let doc = readString [withParseHTML yes, withWarnings no] html
  movie <- parseMovieFromMoviePage url doc
  actors <- parseActorsFromMoviePage doc
  {- persist to DB -}
  withAuthConnection "localhost" 7474 credentials $ do
    B.runBatch $ do
      spr <- addMovieNode movie
      addLabel (T.pack "Movie") spr
      actorNodes <- mapM addActorNode actors
      mapM_ (addLabel (T.pack "Actor")) actorNodes
      mapM_ (createMovieToActorRelationship spr) actorNodes
  putStrLn $ (T.unpack $ title movie) ++ " processed!"

{- Console helpers -}

getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

{- HTML request -}

getHTML :: String -> IO String
getHTML url = simpleHTTP (getRequest url) >>= getResponseBody

{- HTML parsing -}

parseMovieFromMoviePage :: Url -> IOSArrow XmlTree (NTree XNode) -> IO Movie
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


