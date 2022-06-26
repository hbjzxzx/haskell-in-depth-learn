module EvalRPNTrans where

import Control.Monad.State
import Text.Read(readMaybe)
import Control.Applicative (Alternative, empty)
import Data.Foldable

type Stack = [Integer]
type EvalM = StateT Stack Maybe

push :: Integer -> EvalM ()
push x = modify (x:)

pop :: EvalM Integer
pop = do
        xs <- get
        guard (not $ null xs)
        put (tail xs)
        return (head xs)

oneElementOnStack :: EvalM ()
oneElementOnStack = do
        l <- gets length
        guard( l == 1)

readSafe :: (Read a, Alternative m) => String -> m a
readSafe str = maybe empty pure (readMaybe str)
evalRPN :: String -> Maybe Integer
evalRPN expr = evalStateT evalRPN' []
        where
                evalRPN' = traverse_ step (words expr) >> oneElementOnStack >> pop
                step "+" = processTops (+)
                step "*" = processTops (*)
                step "-" = processTops (-)
                step t = readSafe t >>= push
                processTops op = flip op <$> pop <*> pop >>= push

