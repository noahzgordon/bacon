{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Text.HandsomeSoup (css)
import Text.XML.HXT.Core as HXT
import Text.XML.HXT.HTTP
import Text.XML.HXT.TagSoup
import System.IO (hFlush, stdout, stdin, putChar, hGetEcho, hSetEcho)
import Control.Exception (bracket_)
import Control.Monad (filterM)
import Data.Tree.NTree.TypeDefs (NTree)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.List as L

import qualified Database as DB

import Actor (Actor(..))
import Movie (Movie(..))

type Url = String

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

processInitialPage :: DB.Credentials -> Url -> IO ()
processInitialPage credentials url = do
  let doc = getHtmlDocument url
  movie <- parseMovieFromMoviePage url doc
  processMoviePage credentials movie

processMoviePage :: DB.Credentials -> Movie -> IO ()
processMoviePage credentials movie = do
  let doc = getHtmlDocument $ T.unpack $ movieUrl movie
  actors <- parseActorsFromMoviePage doc
  newActors <- filterActors credentials actors
  DB.createMovieWithActors credentials movie newActors
  putStrLn $ (T.unpack $ title movie) ++ " processed!"
  mapM_ (processActorPage credentials) newActors

filterActors :: DB.Credentials -> [Actor] -> IO [Actor]
filterActors creds actors = filterM (DB.hasNoActorNode creds) actors

processActorPage :: DB.Credentials -> Actor -> IO ()
processActorPage credentials actor = do
  let doc = getHtmlDocument $ T.unpack $ actorUrl actor
  movies <- parseMoviesFromActorPage doc
  newMovies <- filterMovies credentials movies
  DB.createActorWithMovies credentials actor newMovies
  putStrLn $ (T.unpack $ name actor) ++ " processed!"
  mapM_ (processMoviePage credentials) newMovies

filterMovies :: DB.Credentials -> [Movie] -> IO [Movie]
filterMovies creds movies = filterM (DB.hasNoMovieNode creds) movies

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
