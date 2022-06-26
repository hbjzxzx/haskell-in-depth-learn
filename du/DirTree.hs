module DirTree where

import AppTypes
import AppRWST
import Utils

import System.FilePath
import System.Directory
import System.PosixCompat.Files

import Control.Monad
import Control.Monad.RWS

dirTree :: MyApp (FilePath, Int) s ()
dirTree = do
  AppEnv {..} <- ask
  fs <- currentPathStatus
  when (isDirectory fs && depth <= maxDepth cfg) $ do
    tell [(takeBaseName path, depth)]
    traverseDirectoryWith dirTree