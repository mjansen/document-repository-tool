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

import Data.Digest.Pure.SHA

file_repository :: FilePath
file_repository = "file-repository"

consideredExtensions            = [ "pdf", "ps", "djvu", "dvi", "tex", "text" ]
consideredCompressionExtensions = [ "gz", "bz2", "xz", "Z" ]

isCandidate :: FilePath -> Bool
isCandidate fName =
  let (rest, ext) = splitExtension fName
  in case ext of
    ""         -> False
    (c:ext') | c == '.'  -> if ext' `elem` consideredCompressionExtensions
                            then isCandidate rest
                            else ext' `elem` consideredExtensions
             | otherwise -> False

main :: IO ()
main = do
  [fsroot] <- getArgs  -- pattern is usually "*.pdf" pdf djvu ps tex dvi and .gz versions
  let repoPath = fsroot </> file_repository
  files <- filter isCandidate . lines <$> readProcess "find" [fsroot, "-type", "f"] ""
  mapM_ print files
  sums <- sort . flip zip files <$> mapConcurrently sha1sum files
  mapM_ print sums
  
  mapM_ (storeFile repoPath) sums
  db <- readRepository repoPath
  let dbS = db `Set.union` Set.fromList sums
  writeRepository repoPath dbS

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

-- the given file pathName is hard linked into the file-repository if
-- we do not already have a copy

storeFile :: FilePath -> (String, String) -> IO ()
storeFile repoPath (sha1, pathName) = do
  let storeDirectory = repoPath </> take 2 sha1
      storePath      = storeDirectory </> sha1
  ok <- doesFileExist storePath
  when (not ok) $ do
    createDirectoryIfMissing True storeDirectory
    createLink pathName storePath
