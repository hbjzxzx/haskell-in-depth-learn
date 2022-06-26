import Data.IORef
import Text.Read (readMaybe)
import GHC.TopHandler (runIO)

sumNumber :: IO Int
sumNumber = do
		s <- newIORef 0
		go s
	where
		go acc = readNumber >>= processNumber acc
		
		readNumber = do
			putStr "Put integer number (not a number to finish): "
			readMaybe <$> getLine
		processNumber acc Nothing = readIORef acc
		processNumber acc (Just n) = modifyIORef' acc (+n) >> go acc

runIORef :: IO ()
runIORef = do
	s <- sumNumber
	putStr "Your sum is: "
	print s