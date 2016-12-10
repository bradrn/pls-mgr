module Main where

import System.Environment
import System.IO
import System.FilePath (splitExtension)
import Data.Function ((&))
import qualified Data.IntMap as Map
import Text.Read

import Parser

main :: IO ()
main = do
    args <- getArgs
    if length args < 2 then printUsage
    else execCommand args

execCommand :: [String] ->  IO ()
execCommand ("add":fileName:title:file:length:_)       = addPls          fileName title file $ maybe (-1) id $ readMaybe length
execCommand ("add-index":fileName:title:file:length:_) = addIndexPls     fileName title file $ maybe (-1) id $ readMaybe length
execCommand ("init":fileName:_)                        = initPls         fileName
execCommand ("move":fileName:_)                        = movePls         fileName
execCommand ("show":fileName:_)                        = showPls         fileName
execCommand ("remove":fileName:_)                      = removeIndexPls  fileName
execCommand ("replace":fileName:title:file:length:_)   = replaceIndexPls fileName title file $ maybe (-1) id $ readMaybe length
execCommand _                                          = printUsage

printUsage :: IO ()
printUsage = hPutStr stderr $ unlines [ "Usage: pls-mgr COMMAND"
                                      , "Commands:"
                                      , "  add playlist_file title file length        Adds a song to the end of a playlist"
                                      , "  add-index playlist_file title file length  Adds a song to a specified index in the playlist"
                                      , "  init playlist_file                         Creates a new playlist"
                                      , "  move playlist_file                         Moves a song at the specified index in the playlist"
                                      , "  remove playlist_file                       Removes a song at the specified index in the playlist"
                                      , "  replace playlist_file title file length    Replaces a song at the specified index in the playlist"
                                      , "  show playlist_file                         Prints the contents of a playlist in a human-readable format" ]

showPls :: String -> IO ()
showPls fileName = do
        contents <- readFile fileName
        let songs = parsePls contents
        printPls songs
  where
    printPls = either (\error -> putStrLn $ "Error: " ++ error)
                      (\songs -> putStrLn "Format: <song-number>: <title> - File <file>, Length <length>" >>
                                 (mapM_ (\(key, value) -> putStrLn ((show key) ++ ": " ++ (pretty value))) $ Map.toAscList songs))

pretty :: Song -> String
pretty Song {songFile=file, songTitle=title, songLength=length} = title ++ " - File \"" ++ file ++ "\", Length " ++ show length

initPls :: String -> IO ()
initPls fileName =
    let fileName' = fileName ++ (if isExtPls then ".pls" else "") in
        writeFile fileName' "[playlist]\nNumberOfEntries=0\nVersion=2"
  where isExtPls = (snd $ splitExtension fileName) /= ".pls"

addPls :: String -> String -> String -> Int -> IO ()
addPls fileName title file length =
    let song = Song { songFile   = file
                    , songTitle  = title
                    , songLength = length
                    }
    in do
        contents <- readFile fileName
        parsePls contents & either (\error -> putStrLn error)
                                   (\songs -> let contents' = toPls $ insertAfter (maxKey songs 1) song songs
                                              in writeFile fileName contents')

addIndexPls :: String -> String -> String -> Int -> IO ()
addIndexPls fileName title file length =
    let song = Song { songFile   = file
                    , songTitle  = title
                    , songLength = length
                    }
    in do
        contents <- readFile fileName
        parsePls contents & either (\error -> putStrLn error)
                                   (\songs -> do
                                        showPls fileName
                                        putStrLn ""
                                        index <- selectIndex "After what song do you want to insert this one?" $ maxKey songs 1
                                        writeFile fileName $ toPls $ insertAfter index song songs)

removeIndexPls :: String -> IO ()
removeIndexPls fileName = do
    contents <- readFile fileName
    parsePls contents & either (\error -> putStrLn error)
                               (\songs -> do
                                    showPls fileName
                                    putStrLn ""
                                    index <- selectIndex "What song do you want to delete?" $ maxKey songs 1
                                    writeFile fileName $ toPls $ removeIndex index songs)

movePls :: String -> IO ()
movePls fileName = do
    contents <- readFile fileName
    parsePls contents & either (\error -> putStrLn error)
                               (\songs -> do
                                    showPls fileName
                                    putStrLn ""
                                    from <- selectIndex "What song do you want to move?"        $ maxKey songs 1
                                    to   <- selectIndex "What index do you want to move it to?" $ maxKey songs 1
                                    case moveIndex from to songs of Just songs'' -> writeFile fileName $ toPls songs''
                                                                    Nothing      -> putStrLn "error")


replaceIndexPls :: String -> String -> String -> Int -> IO ()
replaceIndexPls fileName title file length = 
    let song = Song { songFile   = file
                    , songTitle  = title
                    , songLength = length
                    }
    in do
        contents <- readFile fileName
        parsePls contents & either (\error -> putStrLn error)
                                   (\songs -> do
                                        showPls fileName
                                        putStrLn ""
                                        index <- selectIndex "What song do you want to replace?" $ maxKey songs 1
                                        writeFile fileName $ toPls $ Map.insert index song songs)

selectIndex :: String -> Int -> IO Int
selectIndex message maxIndex = do
    putStr $ message ++ " "
    hFlush stdout
    index <- getLine
    let index' = readMaybe index
    if (maybe False (\num -> 0 <= num && num <= maxIndex) index')
    then return $ maybe (-1) id index'
    else do
        putStrLn "Invalid number"
        selectIndex message maxIndex

maxKey :: Map.IntMap a -> Int -> Int
maxKey map def = if length map == 0 then def else maximum $ Map.keys map

insertAfter :: Map.Key -> a -> Map.IntMap a -> Map.IntMap a
insertAfter index value = Map.insert (index+1) value . Map.fromList . fmap (\(key, val) -> if key > index then (key+1, val) else (key, val)) . Map.toList

removeIndex :: Map.Key -> Map.IntMap a -> Map.IntMap a
removeIndex index =  Map.fromList . fmap (\(key, val) -> if key >= index then (key-1, val) else (key, val)) . Map.toList . Map.delete index

moveIndex :: Map.Key -> Map.Key -> Map.IntMap a -> Maybe (Map.IntMap a)
moveIndex from to songs | from == to = Just songs
                        | otherwise  = fmap (\song -> insertAfter to song $ removeIndex from songs) $ Map.lookup from songs

toPls :: Map.IntMap Song -> String
toPls songs = unlines $ "[playlist]" : mappedSongs ++ footer
  where
    mappedSongs :: [String]
    mappedSongs = Map.toList songs >>= (\(i, song) -> [ "File"   ++ show i ++ "=" ++ songFile song
                                                      , "Title"  ++ show i ++ "=" ++ songTitle song
                                                      , "Length" ++ show i ++ "=" ++ show (songLength song)
                                                      , "" ])   -- empty line for formatting purposes

    footer = [ "NumberOfEntries=" ++ show (length songs)
             , "Version=2" ]
