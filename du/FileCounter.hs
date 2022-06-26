module FileCounter where

import AppTypes
import AppRWST
import Control.Monad
import Control.Monad.RWS
import Utils
import System.PosixCompat
import System.Directory
import System.Directory.Extra
import System.FilePath

fileCount :: MyApp (FilePath, Int) s ()
fileCount = do
	AppEnv {..} <- ask
	fs <- currentPathStatus
	when (isDirectory fs  && depth <= maxDepth cfg) $ do
		traverseDirectoryWith fileCount
		files <- liftIO $ listFiles path
		tell [(path, length $ filter (checkExtension cfg) files)]