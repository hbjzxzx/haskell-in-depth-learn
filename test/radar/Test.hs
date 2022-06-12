{-# LANGUAGE StandaloneDeriving #-}

import System.Exit(exitFailure)
import Data.List (sort, nub)
import Control.Monad (replicateM, unless)
import System.Random
import System.Random.Stateful(uniformRM, uniformM)
import Radar
    ( every,
      orient,
      orientMany,
      rotateMany,
      rotateMany',
      rotateManyStep,
      Direction(..),
      Turn(..) )

-- deriving instance Ord Turn
instance UniformRange Direction where
    uniformRM (lo, hi) rng = do
        res <- uniformRM (fromEnum lo :: Int, fromEnum hi :: Int) rng
        return $ toEnum res
instance Uniform Direction where
    uniformM = uniformRM (minBound, maxBound)

instance UniformRange Turn where
    uniformRM (lo, hi) rng = do
        res <- uniformRM (fromEnum lo :: Int, fromEnum hi :: Int) rng
        return $ toEnum res
instance Uniform Turn where
    uniformM = uniformRM (minBound, maxBound)

deriving instance Ord Turn

deriving instance Ord Direction


uniformIO :: Uniform a => IO a
uniformIO = getStdRandom uniform

uniformsIO :: Uniform a => Int -> IO [a]
uniformsIO n = replicateM n uniformIO

randomDirections :: Int -> IO [Direction]
randomDirections = uniformsIO

randomTurn :: Int -> IO [Turn]
randomTurn = uniformsIO

test_allTurnsInUse :: Bool
test_allTurnsInUse = sort (nub [orient d1 d2 | d1 <- every, d2 <- every]) == every

test_rotationsMonoidAgree :: [Turn] -> Bool
test_rotationsMonoidAgree ts =
    and [ rotateMany d ts == rotateMany' d ts | d <- every ]

test_orientRotateAgree :: [Direction] -> Bool
test_orientRotateAgree [] = True
test_orientRotateAgree ds@(d: _) = ds == rotateManyStep d (orientMany ds)

main :: IO ()
main = do
    ds <- randomDirections 1000
    ts <- randomTurn 1000
    unless (
        test_allTurnsInUse &&
        test_orientRotateAgree ds &&
        test_rotationsMonoidAgree ts)
        exitFailure
