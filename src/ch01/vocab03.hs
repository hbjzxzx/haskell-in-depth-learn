{-# LANGUAGE OverloadedStrings #-}
import Data.Char
import Data.List(group, sort, sortOn, union)
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment ( getArgs )
import qualified GHC.Read as TIO
import Data.Ord
import Fmt
import Control.Monad (when)

type Entry = (Text, Int)
type Vocabulary = [Entry]

extractVocab :: Text -> Vocabulary
extractVocab t = buildEntry <$> group (sort wordList)
    where
        wordList = T.toCaseFold <$>
                    filter (not . T.null)
                        (T.dropAround (not . isLetter ) <$> T.words t)
        buildEntry xs@(x:_) = (x, length xs)

wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortOn (Down . snd)

wordsCountReport :: Vocabulary -> Text
wordsCountReport vs = fmt $ "Total number of words: " +|total|+
                            "\nNumber of unique words: " +|unique|+ "\n"
    where 
        total = sum $ snd <$> vs
        unique = length vs

frequentWordsReport :: Vocabulary -> Int ->Text 
frequentWordsReport vs num = 
    fmt $ nameF "Frequent words"
        $ blockListF' "" fmtEntry reportData
    where
        reportData = take num (wordsByFrequency vs) 
        fmtEntry (t, n) = ""+|t|+": " +|n|+""

allWordsReport :: Vocabulary -> Text
allWordsReport vs = 
    fmt $ nameF "All words" $ unlinesF allWords
    where 
        allWords = fst <$> vs

printAllWords :: Vocabulary -> IO ()
printAllWords vs = do
    putStrLn "All words:"
    TIO.putStrLn $ T.unlines (fst <$> vs)

processTextFile :: FilePath -> Bool -> Int -> IO ()
processTextFile fname withAllWords n = do
    text <- TIO.readFile fname
    let vocab = extractVocab text
    when withAllWords $ TIO.putStrLn $ allWordsReport vocab
    TIO.putStrLn $ wordsCountReport vocab
    TIO.putStrLn $ frequentWordsReport vocab n

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-a", fname, num] -> processTextFile fname True (read num)
        [fname, num] -> processTextFile fname False (read num)
        _ -> putStrLn "Usage: vocab03 [-a] filename freq_num"

