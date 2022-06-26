import Control.Monad.ST
import Data.STRef
import Data.Foldable
import Control.Monad

comp1 :: ST s (STRef s Int)
comp1 = newSTRef 42

comp2 :: STRef s Int -> ST s Int
comp2 = readSTRef

countZerosST :: [Int] -> Int
countZerosST xs = runST $ do
	c <- newSTRef 0
	traverse_ (\x -> when (x==0) $ inc c) xs
	readSTRef c
	where
		inc c = modifySTRef' c (+1)