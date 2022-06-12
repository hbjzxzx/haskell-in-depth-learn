{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Radar where
import Fmt
import Data.List
import Data.Char


data Direction = North | East | South | West
    deriving (Eq, Enum, Bounded, Show)
instance CyclicEnum Direction

data Turn = TNone | TLeft | TRight | TAround
    deriving (Eq, Enum, Bounded, Show)

instance Semigroup Turn where
    TNone <> t = t
    TLeft <> TLeft = TAround
    TLeft <> TRight = TNone
    TLeft <> TAround = TRight
    TRight <> TRight = TAround
    TRight <> TAround = TLeft
    TAround <> TAround = TNone
    t1 <> t2 = t2 <> t1

instance Monoid Turn where
    mempty = TNone

every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred d
        | d == minBound = maxBound
        | otherwise = pred d
    csucc :: a -> a
    csucc d
        | d == maxBound = minBound
        | otherwise = succ d



rotate :: Turn -> Direction -> Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
rotate TAround = cpred . cpred

orient :: Direction -> Direction -> Turn
orient d1 d2 = head $ filter (\t -> rotate t d1 == d2) every

rotateMany :: Direction -> [Turn] -> Direction
rotateMany = foldl (flip rotate)
rotateMany' :: Direction -> [Turn] -> Direction
rotateMany' dir ts = rotate (mconcat ts) dir

rotateManyStep :: Direction -> [Turn] -> [Direction]
rotateManyStep = scanl (flip rotate)

orientMany :: [Direction] -> [Turn]
orientMany ds@(_:_:_) = zipWith orient ds (tail ds)
orientMany _ = []

deriving instance Read Direction
deriving instance Read Turn

instance Buildable Direction where
    build North = "N"
    build East = "E"
    build South = "S"
    build West = "W"

instance Buildable Turn where
    build TNone = "--"
    build TLeft = "<-"
    build TRight = "->"
    build TAround = "||"

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile dir fname = do
    f <- readFile fname
    let
        turns = read <$> lines f
        finalDir = rotateMany' dir turns
        dirs = rotateManyStep dir turns
    fmt $ unwordsF turns
    fmtLn $ "Final direction: " +|finalDir|+ ""
    fmt $ nameF "Intermediate directions" (unwordsF dirs)

orientFromFile :: FilePath -> IO ()
orientFromFile fname = do
    f <- readFile fname
    let
        dirs = read <$> lines f
        ts = orientMany dirs
    fmt $ nameF "All turns: " (unwordsF ts)






