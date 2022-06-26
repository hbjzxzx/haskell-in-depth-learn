module DiskUsage where

import AppRWST
import AppTypes
import Control.Monad (liftM2)
import Control.Monad.RWS
import System.Directory
import System.Posix.Types
import System.PosixCompat (fileSize)
import System.PosixCompat.Files
import Utils (checkExtension, currentPathStatus, traverseDirectoryWith)

data DUEntryAction
  = TraverseDir {dirPath :: FilePath, requireReporting :: Bool}
  | RecordFileSize {fsize :: FileOffset}
  | None

diskUsage :: MyApp (FilePath, FileOffset) FileOffset ()
diskUsage = liftM2 decide ask currentPathStatus >>= processEntry
  where
    decide AppEnv {..} fs
      | isDirectory fs =
          TraverseDir path (depth <= maxDepth cfg)
      | isRegularFile fs && checkExtension cfg path =
          RecordFileSize (fileSize fs)
      | otherwise = None
    processEntry TraverseDir {..} = do
      usageOnEntry <- get
      traverseDirectoryWith diskUsage
      when requireReporting $ do
        usageOnExit <- get
        tell [(dirPath, usageOnExit - usageOnEntry)]
    processEntry RecordFileSize {fsize} = modify (+fsize)
    processEntry None = pure ()