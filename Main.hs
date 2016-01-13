{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Text.HandsomeSoup (css)
import Text.XML.HXT.Core as HXT
import Text.XML.HXT.HTTP
import Text.XML.HXT.TagSoup
import Data.Tree.NTree.TypeDefs (NTree)
import System.IO (hFlush, stdout, stdin, putChar, hGetEcho, hSetEcho)
import Control.Exception (bracket_)
import Database.Neo4j as Neo
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import qualified Data.List as L
import qualified Database.Neo4j.Batch as B

type Url = String

data Actor = Actor { name :: T.Text, actorUrl :: T.Text }
data Movie = Movie { title :: T.Text, movieUrl :: T.Text }

main :: IO ()
main = do
  putStrLn "In order to use this program, you must run a Neo4j server on localhost:7474."
  putStrLn "If you are not already doing so, start it up now.\n"
  putStrLn "What is your Neo4j server username?"
  username <- getLine
  putStrLn "What is your Neo4j server password?"
  password <- getPassword
  putStrLn "Please provide an IMDB movie url from which to start the Baconator."
  url <- getLine
  processInitialPage (BS.pack username, BS.pack password) url

processInitialPage :: Credentials -> Url -> IO ()
processInitialPage credentials url = do
  let doc = getHtmlDocument url
  movie <- parseMovieFromMoviePage url doc
  Neo.withAuthConnection "localhost" 7474 credentials $ do
    B.runBatch $ do
      movieNode <- addMovieNode movie
      addLabel (T.pack "Movie") movieNode
  processMoviePage credentials movie

processMoviePage :: Credentials -> Movie -> IO ()
processMoviePage credentials movie = do
  let doc = getHtmlDocument $ T.unpack $ movieUrl movie
  actors <- parseActorsFromMoviePage doc
  Neo.withAuthConnection "localhost" 7474 credentials $ do
    B.runBatch $ do
      movieNode <- addMovieNode movie
      addLabel (T.pack "Movie") movieNode
      actorNodes <- mapM addActorNode actors
      mapM_ (addLabel "Actor") actorNodes
      mapM_ (createMovieToActorRelationship movieNode) actorNodes
      return movieNode
  putStrLn $ (T.unpack $ title movie) ++ " processed!"
  mapM_ (processActorPage credentials) actors


processActorPage :: Credentials -> Actor -> IO ()
processActorPage credentials actor = do
  let doc = getHtmlDocument $ T.unpack $ actorUrl actor
  movies <- parseMoviesFromActorPage doc
  Neo.withAuthConnection "localhost" 7474 credentials $ do
    B.runBatch $ do
      actorNode <- addActorNode actor
      addLabel "Actor" actorNode
      movieNodes <- mapM addMovieNode movies
      mapM_ (addLabel "Movie") movieNodes
      mapM_ (createActorToMovieRelationship actorNode) movieNodes
  putStrLn $ (T.unpack $ name actor) ++ " processed!"

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

getHtmlDocument :: String -> IOStateArrow a b XmlTree
getHtmlDocument = readDocument [ withParseHTML yes
                               , withWarnings  no
                               , withRemoveWS  yes
                               , withValidate  yes
                               , withTagSoup
                               , withHTTP []
                               ]

{- HTML parsing -}

parseMovieFromMoviePage :: Url -> HXT.IOSArrow HXT.XmlTree (NTree HXT.XNode) -> IO Movie
parseMovieFromMoviePage url tree = do
  title <- fmap head $ runX $ getMovieTitle tree
  return Movie { title = (T.pack title), movieUrl = (T.pack url) }

getMovieTitle :: HXT.IOSArrow HXT.XmlTree (NTree HXT.XNode) -> HXT.IOSLA (HXT.XIOState ()) HXT.XmlTree String
getMovieTitle tree = tree //> hasAttrValue "class" (== "header") /> hasAttrValue "itemprop" (== "name") /> getText

parseActorsFromMoviePage :: HXT.IOSArrow HXT.XmlTree (NTree HXT.XNode) -> IO [Actor]
parseActorsFromMoviePage tree = do
  actorProps <- runX $ tree //> hasAttrValue "itemprop" (== "actor") >>> (parseActorName &&& parseActorUrl)
  return $ map (\(name,url) -> Actor { name = (T.pack name), actorUrl = (T.pack $ "http://www.imdb.com" ++ url) }) actorProps

parseActorName :: HXT.IOSLA (HXT.XIOState ()) HXT.XmlTree String
parseActorName = deep $ hasAttrValue "itemprop" (== "name") /> getText

parseActorUrl :: HXT.IOSLA (HXT.XIOState ()) HXT.XmlTree String
parseActorUrl = deep $ hasAttrValue "itemprop" (== "url") >>> getAttrValue "href"

parseMoviesFromActorPage :: HXT.IOSArrow HXT.XmlTree (NTree HXT.XNode) -> IO [Movie]
parseMoviesFromActorPage tree = do
  movieProps <- runX $ tree >>> css ".filmo-category-section" /> css ".filmo-row[id|=actor]" /> 
    hasName "b" /> hasName "a"  >>> (deep getText &&& getAttrValue "href")
  return $ map (\(title,url) -> Movie { title = (T.pack title), movieUrl = (T.pack $ "http://www.imdb.com" ++ url) }) movieProps

parseMovieTitle :: HXT.IOSLA (HXT.XIOState ()) HXT.XmlTree String
parseMovieTitle = deep $ hasName "a" /> getText

parseMovieUrl :: HXT.IOSLA (HXT.XIOState ()) HXT.XmlTree String
parseMovieUrl = deep $ hasName "a" >>> getAttrValue "href"

{- DB Interaction -}

getMovieNodes :: Movie -> B.Batch (B.BatchFuture [Node])
getMovieNodes movie = do
  B.getNodesByLabelAndProperty "Movie" (Just ("title" |: title movie))

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

createActorToMovieRelationship :: B.BatchFuture Node -> B.BatchFuture Node -> B.Batch (B.BatchFuture Relationship)
createActorToMovieRelationship = flip createMovieToActorRelationship

addLabel :: Label -> B.BatchFuture Node -> B.Batch (B.BatchFuture ())
addLabel label node = B.addLabels [label] node

