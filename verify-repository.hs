import System.Process
import GHC.Exts
import Data.List
import System.FilePath
import Control.Applicative
import Control.Monad
import System.Directory
import System.Posix.Files
import System.Environment
import Control.Concurrent.Async

import Data.Set (Set)
import qualified Data.Set as Set
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Serialize
import GHC.Generics
import Text.Printf

import Data.Digest.Pure.SHA

file_repository :: FilePath
file_repository = "file-repository"

main :: IO ()
main = do
  [fsroot] <- getArgs  -- pattern is usually "*.pdf" pdf djvu ps tex dvi and .gz versions
  let repoPath = fsroot </> file_repository
  files <- lines <$> readProcess "find" [repoPath, "-type", "f"] ""
  putStrLn "checking ..."
  mapM_ checkFile files
  putStrLn "done"

checkFile fPath = do
  let (_, fName) = splitFileName fPath
  if length fName /= 40
    then return ()
    else do
      csum <- sha1sum fPath
      if csum /= fName
        then printf "content of %s has checksum %s, which does not match\n" fPath csum
        else return ()

-- use the pure implementation of SHA to compute the sha1sum of a file:

sha1sum :: FilePath -> IO String
sha1sum fName = show . sha1 <$> L.readFile fName

-- load repository data base form a binary file:

readRepository :: FilePath -> IO (Set (String, String))
readRepository path = do
  let indexFile = path </> "index"
  exists <- doesFileExist indexFile
  if exists
    then either (const Set.empty) id . decode <$> B.readFile indexFile
    else return Set.empty

writeRepository :: FilePath -> Set (String, String) -> IO ()
writeRepository path = B.writeFile (path </> "index") . encode
