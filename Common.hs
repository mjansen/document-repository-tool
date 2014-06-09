module Common where

-- import System.Process
-- import GHC.Exts
import Data.Char
-- import Data.List
import System.FilePath
import Control.Applicative
import Control.Monad
import System.Directory
import System.Posix.Files
-- import System.Environment
-- import Control.Concurrent.Async

import Data.Set (Set)
import qualified Data.Set as Set
-- import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Serialize
-- import GHC.Generics

import Data.Digest.Pure.SHA

file_repository :: FilePath
file_repository = "file-repository"

-- specify these in lower case:

consideredExtensions :: [String]
consideredExtensions            = [ "pdf", "ps", "djvu", "dvi", "tex", "text" ]

consideredCompressionExtensions :: [String]
consideredCompressionExtensions = [ "gz", "bz2", "xz", "z" ]

isCandidate :: FilePath -> Bool
isCandidate fName =
  let (rest, ext) = splitExtension fName
  in case ext of
    ""         -> False
    (c:ext') | c == '.'  -> if map toLower ext' `elem` consideredCompressionExtensions
                            then isCandidate rest
                            else map toLower ext' `elem` consideredExtensions
             | otherwise -> False

-- use the pure implementation of SHA to compute the sha1sum of a file:

sha1sum :: FilePath -> IO String
sha1sum fName = show . sha1 <$> L.readFile fName

-- load repository data base form a binary file:

readRepository :: FilePath -> IO (Set (String, String))
readRepository path = readIndexFile (path </> "index")

readIndexFile :: FilePath -> IO (Set (String, String))
readIndexFile indexFile = do
  exists <- doesFileExist indexFile
  if exists
    then either (const Set.empty) id . decode <$> B.readFile indexFile
    else return Set.empty

writeRepository :: FilePath -> Set (String, String) -> IO ()
writeRepository path = writeIndexFile (path </> "index")

writeIndexFile :: FilePath -> Set (String, String) -> IO ()
writeIndexFile indexFile = B.writeFile indexFile . encode

mergeIndexes :: [FilePath] -> FilePath -> IO ()
mergeIndexes fNames destination =
  (Set.unions <$> mapM readIndexFile (destination:fNames)) >>= writeIndexFile destination  

-- the given file pathName is hard linked into the file-repository if
-- we do not already have a copy

storeFile :: FilePath -> (String, String) -> IO ()
storeFile repoPath (csum, pathName) = do
  let storeDirectory = repoPath </> take 2 csum
      storePath      = storeDirectory </> csum
  ok <- doesFileExist storePath
  when (not ok) $ do
    createDirectoryIfMissing True storeDirectory
    createLink pathName storePath
