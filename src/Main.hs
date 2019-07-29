{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Foldable (for_, traverse_)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.Torrent
import           System.Environment (getArgs)
import           Control.Exception (SomeException, try, evaluate)
import           Control.Monad (join)


main :: IO ()
main = getArgs >>= app

app :: [FilePath] -> IO ()
app [] = undefined
app [x] = run' x
app xs = for_ xs $ \x -> do
    putStr x
    putStrLn [':']
    run' x
    putStrLn []

run' :: FilePath -> IO ()
run' fp = do
    d <- B.readFile fp
    readTorrent' d >>= \case
        Left e -> print e
        Right t -> run $ tInfo t

readTorrent' :: ByteString -> IO (Either SomeException Torrent)
readTorrent' d =
    try $ join $ evaluate $
        case readTorrent d of
            Left e -> fail e
            Right t -> pure t

run :: TorrentInfo -> IO ()
run SingleFile { tName } = printSingleFile tName
run MultiFile { tName, tFiles } = printMultiFile tName tFiles

printMultiFile :: ByteString -> [TorrentFile] -> IO ()
printMultiFile = traverse_ . printFilePath

printSingleFile :: ByteString -> IO ()
printSingleFile x = do
    putChar '/'
    B.putStr x
    putStrLn []

printFilePath :: ByteString -> TorrentFile -> IO ()
printFilePath name tf = do
    for_ (filePath tf) $ \x -> do
        putChar '/'
        B.putStr name
        putChar '/'
        B.putStr x
    putStrLn []
