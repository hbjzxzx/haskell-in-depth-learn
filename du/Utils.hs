module Utils where

import System.PosixCompat.Files
import System.Directory
import Control.Monad.RWS
import Data.Foldable
import System.FilePath
import AppRWST
import AppTypes

currentPathStatus :: MyApp l s FileStatus
currentPathStatus = do
  AppEnv {fileStatus, path} <- ask
  liftIO $ fileStatus path


traverseDirectoryWith :: MyApp le s () -> MyApp le s ()
traverseDirectoryWith app = do
  curPath <- asks path
  content <- liftIO $ listDirectory curPath
  traverse_ go content
  where
    go name = flip local app $
      \env -> env {
        path = path env </> name,
        depth = depth env + 1
        }


checkExtension :: AppConfig -> FilePath -> Bool
checkExtension cfg fp = 
  maybe True (`isExtensionOf` fp) (extension cfg)