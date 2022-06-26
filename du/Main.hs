module Main where
import AppTypes (AppConfig(AppConfig))
import AppRWST (runMyApp, MyApp)
import DiskUsage
import DirTree
import FileCounter
import System.Posix.Types
import TextShow
import Data.Text.IO as TIO
import Options.Applicative as Opt
import System.Directory (doesFileExist)

import Control.Monad.RWS
import Control.Monad.Writer (mapWriter)

work :: AppConfig -> IO ()
work config = do
  (_, dirs) <- runMyApp dirTree config ()
  (_, counters) <- runMyApp fileCount config ()
  (_, usage) <- runMyApp diskUsage config (0 :: FileOffset)
  let report = toText $ buildEntries "Directory tree: " treeEntryBuilder dirs
                        <> buildEntries "File counter: " tabEntryBuilder counters
                        <> buildEntries "File space usage: " ftabEntryBuilder usage
  TIO.putStr report


buildEntries :: Builder -> (e -> Builder) -> [e] -> Builder
buildEntries title entryBuilder entries =
  unlinesB $ title : (entryBuilder <$> entries)

tabEntryBuilder :: TextShow s => (FilePath, s) -> Builder
tabEntryBuilder (fp, s) = showb s <> "\t" <> fromString fp

ftabEntryBuilder :: (TextShow s, Integral s) => (FilePath, s) -> Builder
ftabEntryBuilder (fp, s) = showb ((fromIntegral s :: Double)/1024/1024) <> "\t" <> fromString fp

treeEntryBuilder :: (FilePath, Int) -> Builder
treeEntryBuilder (fp, n) = fromString indent <> fromString fp
  where
    indent = replicate (2*n) ' '


main :: IO ()
main = execParser opts >>= work
  where
    opts = info (mkConfig <**> helper)
                (fullDesc <> progDesc "Directory usage info")

mkConfig :: Opt.Parser AppConfig
mkConfig =
  AppConfig
  <$> strArgument (metavar "DIRECTORY" <> value "." <> showDefault)
  <*> option auto
    (metavar "DEPTH" <> short 'd' <> long "depth" <> value maxBound <>
      help "Display an entry for all directories DEPTH directories deep")
  <*> optional
    (strOption (metavar "EXT" <> long "extension" <> short 'e' <>
      help "Filter files by extension"))
  <*> switch
      (short 'L' <> help "Follow symlinks (OFF by default)")


