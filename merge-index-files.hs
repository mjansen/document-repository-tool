import System.Environment
import Common

main :: IO ()
main = do
  args <- getArgs
  mergeIndexes (init args) (last args)
