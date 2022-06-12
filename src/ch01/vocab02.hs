import Data.Char
import Data.List(group, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment ( getArgs )
import qualified GHC.Read as TIO

type Entry = (T.Text, Int)
type Vocabulary = [Entry]

extractVocab :: T.Text -> Vocabulary
extractVocab t = buildEntry <$> group (sort wordList)
    where
        wordList = T.toCaseFold <$>
                    filter (not . T.null)
                        (T.dropAround (not . isLetter ) <$> T.words t)
        buildEntry xs@(x:_) = (x, length xs)

printAllWords :: Vocabulary -> IO ()
printAllWords vs = do
    putStrLn "All words:"
    TIO.putStrLn $ T.unlines (fst <$> vs)

processTextFile :: FilePath -> IO ()
processTextFile fname = do
    text <- TIO.readFile fname
    let vocab = extractVocab text
    printAllWords vocab

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fname] -> processTextFile fname
        _ -> putStrLn "Usage: vocab02 filename"

