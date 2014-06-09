import System.Process
-- import GHC.Exts
-- import Data.Char
import Data.List
import System.FilePath
import Control.Applicative
-- import Control.Monad
-- import System.Directory
-- import System.Posix.Files
import System.Environment
import Control.Concurrent.Async

-- import Data.Set (Set)
import qualified Data.Set as Set
-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as L
-- import Data.Serialize
-- import GHC.Generics

-- import Data.Digest.Pure.SHA

import Common

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
