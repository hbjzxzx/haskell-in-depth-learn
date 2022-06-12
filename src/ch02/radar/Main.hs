module Main where
import System.Environment (getArgs)
import Radar
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-r", fname, dir] -> rotateFromFile (read dir) fname
        ["-o", fname] -> orientFromFile fname 
        _ -> putStrLn $ "Usage: localtor -o filename\n" ++ "localtor -r filename direction" 