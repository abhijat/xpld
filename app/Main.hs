module Main where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Monad (forM)
import qualified Data.ByteString.Lazy as BL
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath (takeDirectory, takeFileName, (</>))

main :: IO ()
main =
  findRedpandaLogsHere >>= \logs ->
    putStrLn ("found log files " <> show logs)
      >> mapM_ extractInPlace logs

lsRec :: FilePath -> IO [FilePath]
lsRec topLvl = do
  names <- getDirectoryContents topLvl
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames processPathName
  return $ concat paths
  where
    processPathName name = do
      let path = topLvl </> name
      isDir <- doesDirectoryExist path
      if isDir
        then lsRec path
        else return [path]

findPath :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findPath predicate rootPath = do
  names <- lsRec rootPath
  return (filter predicate names)

findRedpandaLogsHere :: IO [FilePath]
findRedpandaLogsHere = findPath (isLogFile . takeFileName) "."
  where
    isLogFile f = f == "redpanda.log.tgz" || f == "redpanda.log.tar.gz"

extractLog :: FilePath -> FilePath -> IO ()
extractLog logFile destDir = do
  putStrLn ("extracting " <> logFile <> " to " <> destDir)
  BL.readFile logFile
    >>= Tar.unpack destDir . Tar.read . GZip.decompress . GZip.decompress

extractInPlace :: FilePath -> IO ()
extractInPlace filePath = extractLog filePath $ takeDirectory filePath
