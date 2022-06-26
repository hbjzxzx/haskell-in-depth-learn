module Main where

import QuoteData
import qualified Data.ByteString.Lazy as BL
import Data.Csv (decodeByName)
import Data.Foldable (toList)


readQuotes :: FilePath -> IO [QuoteData]
readQuotes fpath = do
    csvData <- BL.readFile fpath
    case decodeByName csvData of
        Left err -> error err
        Right (_, quotes) -> return (toList quotes)

main :: IO()
main = undefined