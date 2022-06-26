import System.Random
import System.Random.Stateful
import Control.Monad.State
import Data.List (group, sort)
-- import Data.

data Weapon = Rock | Paper | Scissors
    deriving (Show, Bounded, Enum, Eq)

data Winner = First | Second | Draw
    deriving (Show, Eq, Ord)

winner :: (Weapon, Weapon) -> Winner
winner (Paper, Rock) = First
winner (Scissors, Paper) = First
winner (Rock, Scissors) = First
winner (w1, w2)
    | w1 == w2 = Draw
    | otherwise = Second
    
instance UniformRange Weapon where
    uniformRM (lo, hi) rng = do
        res <- uniformRM (fromEnum lo :: Int, fromEnum hi) rng
        pure $ toEnum res

instance Uniform Weapon where
    uniformM rng = do
        uniformRM (minBound, maxBound) rng


randomWeapon :: State StdGen Weapon
randomWeapon = state uniform

gameRound :: State StdGen (Weapon, Weapon)
gameRound = (, ) <$> randomWeapon <*> randomWeapon

game :: Int -> State StdGen [(Winner, Int)]
game n = counts <$> replicateM n (winner <$> gameRound) 
    where
        headLength xs@(x:_) = (x, length xs)
        counts xs = map headLength $ group $ sort xs
        

runGame :: IO ()
runGame = do
    g <- newStdGen
    let r = evalState (game 10) g
    print r