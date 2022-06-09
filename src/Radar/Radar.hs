{-# LANGUAGE DerivedAnyClass #-}

module Radar
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred d =
        | d == minBound = maxBound
        | otherwise = prec d
    csucc :: a -> a
    csucc d =
        | d == maxBound = minBound
        | otherwise = succ d


data Turn = TNone | TLeft | TRight | TAround
    deriving(Eq, Enum, Bounded, Show, CyclicEnum)
data Direction = North | East | South | West
    deriving(Eq, Enum, Bounded, Show, CyclicEnum)

rotate :: Turn -> Direction -> Direction
orient :: Direction -> Direction -> Turn

rotateMany :: Direction -> [Turn] -> Direction
rotateManyStep :: Direction -> [Turn] -> [Direction]

orientMany :: [Directoin] -> [Turn]

rorateFromFile :: Direction -> FilePath -> IO()
orientFromFile :: FilePath -> IO()


every :: (Enum a, Bounded a) => [a]
every = EnumFrom minBound
