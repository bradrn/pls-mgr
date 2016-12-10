{-# LANGUAGE FlexibleContexts #-}
module Parser
( Song(..)
, parsePls
) where

import Control.Monad
import Data.Either
import Data.Function ((&))
import Data.Functor.Identity
import qualified Data.IntMap.Strict as Map
import Data.List
import Data.List.Split
import Safe
import Text.Parsec
import Text.Read

data Song = Song { songFile   :: FilePath
                 , songTitle  :: String
                 , songLength :: Int
                 }

parsePls :: String -> Either String (Map.IntMap Song)
parsePls = getSongs <=< strip . filter (/= "") . lines

strip :: [String] -> Either String [String]
strip = stripHeader >=> stripFooter
   where
     stripHeader :: [String] -> Either String [String]
     stripHeader plsLines = if maybe False (/= "[playlist]") $ headMay plsLines
                            then Left "First line needs to be [playlist]"
                            else maybe (Left "Needs to have >1 lines") Right $ tailMay plsLines

     stripFooter :: [String] -> Either String [String]
     stripFooter plsLines = do
       let plsLast = lastMay plsLines
       when (maybe True (/= "Version=2") $ fmap removeWhitespace plsLast)
          $ Left "Last line must be 'Version=2'"

       let plsLast' = if length plsLines > 2                                                               -- plsLast' is second last line of plsLines
                      then plsLines & tailMay >>= initMay >>= lastMay
                      else headMay plsLines
       when (maybe True (not . ("NumberOfEntries=" `isPrefixOf`)) $ fmap removeWhitespace plsLast')
          $ Left "Second last line needs to be 'NumberOfEntries=<number of entries>'"

       (plsLines & initMay >>= initMay) & maybe (Left "Wrong last two lines") Right                       -- return all except last two lines
           where removeWhitespace = concat . words

getSongs :: [String] -> Either String (Map.IntMap Song)
getSongs plsFile = sequence (toList <$> plsFile) >>= toSongs

toList :: String -> Either String (String, String)
toList body = if length splitted >= 2 then Right (head splitted, join $ intersperse "=" $ tail splitted)  -- we need `join $ intersperse "=" $ tail splitted` for if
                                                                                                          -- we get an input with an '=' in it
                                      else Left "Lines should be of the form '<value1>=<value2>'"
    where splitted = splitOn "=" body

toSongs :: [(String, String)] -> Either String (Map.IntMap Song)
toSongs = foldl updateSongMap $ Right Map.empty

updateSongMap :: Either String (Map.IntMap Song) -> (String, String) -> Either String (Map.IntMap Song)
updateSongMap map val = do
    songMap <- map
    (getNumber $ fst val) & either (\perr       -> Left $ show perr ++ "\nin " ++ fst val)
                                   (\songNumber -> maybe (Left "Keys should be of the form '<text><number>'") (process songMap) songNumber)

  where
    process :: Map.IntMap Song -> Map.Key -> Either String (Map.IntMap Song)
    process songMap songNumber = if Map.member songNumber songMap
                                 then let song  = Map.lookup songNumber songMap
                                          song' = maybe (Left "") (addToSong val) song
                                      in song' & either (\error  -> Left error)
                                                        (\song'' -> Right $ Map.adjust (const song'') songNumber songMap)
                                 else (addToSong val defaultSong) >>= (\song' -> Right $ Map.insert songNumber song' songMap)

defaultSong :: Song
defaultSong = Song { songFile = ""
                   , songTitle = ""
                   , songLength = -1
                   }

getNumber :: (Stream s Identity Char, Read a) => s -> Either ParseError (Maybe a)
getNumber key = parse (many1 (noneOf "0123456789") *> many1 digit) "" key >>= Right . readMaybe

addToSong :: (String, String) -> Song -> Either String Song
addToSong (key, val) song | "File"   `isPrefixOf` key = Right $ song { songFile = val }
                          | "Title"  `isPrefixOf` key = Right $ song { songTitle = val }
                          | "Length" `isPrefixOf` key = maybe (Left "Value of 'Length' should be an integer") (\val -> Right $ song { songLength = val }) $ readMaybe val
                          | otherwise                 = Left "Keys should either be 'File', 'Title' or 'Length'"
