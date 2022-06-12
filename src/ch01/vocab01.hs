import Data.Char
import Data.List(group, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment ( getArgs )
import qualified Data.Text.IO as T

main :: IO ()
main = do
    [fname] <- getArgs
    text <- T.readFile fname
    let
        wordList = T.dropAround (not . isLetter) <$> T.words text
        wordListNoNa = filter (not . T.null ) wordList
        wordListIgnoreCase = T.toCaseFold <$> wordListNoNa
        ws = head <$> group (sort wordListIgnoreCase)
    T.putStrLn $ T.unwords ws
    print $ length ws

