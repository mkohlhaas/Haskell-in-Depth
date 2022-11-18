{-# LANGUAGE OverloadedStrings #-}

module Sessions where

import Control.Monad (unless)
import Control.Monad.Trans (liftIO)
import Data.Int
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import FilmInfoData
import Hasql.Session (Session)
import qualified Hasql.Session as Session
import qualified Statements as Stmt

allFilms ∷ Session (Vector FilmInfo)
allFilms = Session.statement () Stmt.allFilms

countFilms ∷ Session Int64
countFilms = Session.statement () Stmt.countFilms

findFilm ∷ Text → Session (Maybe FilmInfo)
findFilm ttl = Session.statement ttl Stmt.findFilm

filmsLonger ∷ FilmLength → Session (Vector FilmInfo)
filmsLonger len = Session.statement len Stmt.filmsLonger

filmsCategories ∷ [Text] → Session [FilmCategories]
filmsCategories films = catMaybes <$> mapM runSingle films
  where
    runSingle ttl = do
      mfilm ← findFilm ttl
      case mfilm of
        Nothing → pure Nothing
        Just film → do
          cats ← Session.statement ttl Stmt.filmCategories
          pure $ Just $ FilmCategories film $ V.toList cats

setRating ∷ Rating → Text → Session Int64
setRating newRating film = Session.statement (newRating, film) Stmt.setRating

findOrAddCategory ∷ Text → Session CatId
findOrAddCategory catName = do
  cats ← Session.statement catName Stmt.catIdByName
  case cats of
    Nothing → Session.statement catName Stmt.newCategory
    Just cid → pure cid

assignUnlessAssigned ∷ CatId → FilmId → Session Int64
assignUnlessAssigned cid fid = do
  b ← Session.statement (cid, fid) Stmt.isAssigned
  if b
    then pure 0
    else Session.statement (cid, fid) Stmt.assignCategory

assignCategory ∷ Text → Text → Session Int64
assignCategory catName filmTitle = do
  cid ← findOrAddCategory catName
  mFilmId ← Session.statement filmTitle Stmt.filmIdByTitle
  case mFilmId of
    Nothing → pure 0
    Just fid → assignUnlessAssigned cid fid

unassignCategory ∷ Text → Text → Session Int64
unassignCategory catName filmTitle =
  Session.statement (catName, filmTitle) Stmt.unassignCategory

processAllFilms ∷ (FilmInfo → IO ()) → Session ()
processAllFilms process = do
  Session.sql "BEGIN"
  Session.sql declareCursor
  fetchRowsLoop
  Session.sql "END"
  where
    declareCursor =
      "DECLARE films_cursor CURSOR FOR "
        <> "SELECT film_id, title, description, "
        <> "       length, rating FROM film"
    fetchRowsLoop = do
      rows ← Session.statement () Stmt.fetchFilmsChunk
      unless (V.null rows) $ do
        liftIO (V.mapM_ process rows)
        fetchRowsLoop
