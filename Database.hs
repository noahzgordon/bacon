{-# LANGUAGE OverloadedStrings #-}
module Database where

import Safe (headMay)
import qualified Data.Text as T

import Database.Neo4j.Graph     (Graph)
import qualified Database.Neo4j        as N
import qualified Database.Neo4j.Batch  as B
import qualified Data.HashMap.Lazy     as M

import Actor (Actor, name, actorUrl)
import Movie (Movie, title, movieUrl)

type Credentials = N.Credentials


createMovie :: N.Credentials -> Movie -> IO N.Node
createMovie creds movie =
  N.withAuthConnection "localhost" 7474 creds $ do
    movieNode <- addMovieNode movie
    addLabel "Movie" movieNode
    return movieNode

createMovieWithActors :: N.Credentials -> Movie -> [Actor] -> IO Graph
createMovieWithActors creds movie actors =
  N.withAuthConnection "localhost" 7474 creds $ do
    B.runBatch $ do
      newMovieNode <- batchAddMovieNode movie
      batchAddLabel "Movie" newMovieNode
      actorNodes <- mapM batchAddActorNode actors
      mapM_ (batchAddLabel "Actor") actorNodes
      mapM_ (batchCreateMovieToActorRelationship newMovieNode) actorNodes
      return newMovieNode

createActorWithMovies :: N.Credentials -> Actor -> [Movie] -> IO Graph
createActorWithMovies creds actor movies =
  N.withAuthConnection "localhost" 7474 creds $ do
    B.runBatch $ do
      newActorNode <- batchAddActorNode actor
      batchAddLabel "Actor" newActorNode
      movieNodes <- mapM batchAddMovieNode movies
      mapM_ (batchAddLabel "Movie") movieNodes
      mapM_ (batchCreateActorToMovieRelationship newActorNode) movieNodes
      return newActorNode

fetchMovieNode :: N.Credentials -> Movie -> IO (Maybe N.Node)
fetchMovieNode creds movie =
  N.withAuthConnection "localhost" 7474 creds $ do
    movieNodes <- N.getNodesByLabelAndProperty "Movie" (Just ("title" N.|: title movie))
    return $ headMay movieNodes

fetchActorNode :: N.Credentials -> Actor -> IO (Maybe N.Node)
fetchActorNode creds actor =
  N.withAuthConnection "localhost" 7474 creds $ do
    actorNodes <- N.getNodesByLabelAndProperty "Actor" (Just ("name" N.|: name actor))
    return $ headMay actorNodes

addMovieNode :: Movie -> N.Neo4j N.Node
addMovieNode movie =
  N.createNode $ M.fromList [ "title" N.|: title movie
                            , "url"   N.|: movieUrl movie
                            ]

batchAddMovieNode :: Movie -> B.Batch (B.BatchFuture N.Node)
batchAddMovieNode movie =
  B.createNode $ M.fromList [ "title" N.|: title movie
                            , "url"   N.|: movieUrl movie
                            ]

batchAddActorNode :: Actor -> B.Batch (B.BatchFuture N.Node)
batchAddActorNode actor =
  B.createNode $ M.fromList [ "name"  N.|: name actor
                            , "url"   N.|: actorUrl actor
                            ]

batchCreateMovieToActorRelationship :: B.BatchFuture N.Node -> B.BatchFuture N.Node -> B.Batch (B.BatchFuture N.Relationship)
batchCreateMovieToActorRelationship movieNode actorNode =
  B.createRelationship "ACTED_IN" M.empty actorNode movieNode

batchCreateActorToMovieRelationship :: B.BatchFuture N.Node -> B.BatchFuture N.Node -> B.Batch (B.BatchFuture N.Relationship)
batchCreateActorToMovieRelationship = flip batchCreateMovieToActorRelationship

addLabel :: N.Label -> N.Node -> N.Neo4j ()
addLabel label node = N.addLabels [label] node

batchAddLabel :: N.Label -> B.BatchFuture N.Node -> B.Batch (B.BatchFuture ())
batchAddLabel label node = B.addLabels [label] node

