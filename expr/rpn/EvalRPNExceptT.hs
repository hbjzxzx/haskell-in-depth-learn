{-# LANGUAGE OverloadedStrings #-}

module EvalRPNExceptT where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read
import TextShow
import Data.Char
import Data.Foldable

import qualified Data.Char as T
import GHC.Conc (reportError)
import qualified Data.Text.IO as T
import qualified Data.Text.IO as TIO
import System.Exit
type Stack = [Integer]
type EnvVars = [(Text, Integer)]

data EvalError = 
    NotEnoughElements
  | ExtraElements
  | NotANumber Text
  | UnKnownVar Text 

instance TextShow EvalError where
  showb NotEnoughElements = "Not enough elements in the expression"
  showb ExtraElements = "There are extra elements in the expression" 
  showb (NotANumber t) = "Expression component: '" <> fromText t <> "' is not a number"
  showb (UnKnownVar t) = "Variable '" <> fromText t <> "' not found" 


type EvalM = ReaderT EnvVars (ExceptT EvalError (State Stack))

push :: Integer -> EvalM ()
push x = modify' (x:)

pop :: EvalM Integer
pop = get >>= pop'
  where
    pop' :: Stack -> EvalM Integer
    pop' [] = throwError NotEnoughElements
    pop' (x:xs) = put xs >> pure x


readVar :: Text -> EvalM Integer
readVar name = do
  var <- asks (lookup name)
  case var of 
    Nothing -> throwError $ UnKnownVar name
    Just n -> pure n


readNumber :: Text -> EvalM Integer
readNumber txt = do
  case decimal txt of
    Right (n, rest) | T.null rest -> pure n
    _ -> throwError $ NotANumber txt

readSafe :: Text -> EvalM Integer
readSafe t 
  | isNum t = readNumber t
  | otherwise = readVar t
  where
    isNum t = maybe False (T.isNumber  . fst) (T.uncons t)

oneElementOnStack :: EvalM ()
oneElementOnStack = do
  len <- gets length
  when (len /= 1) $ throwError ExtraElements


evalRPNOnce :: Text -> EvalM Integer
evalRPNOnce str = clearStack >> traverse_ step (T.words str) >> oneElementOnStack >> pop
  where
    clearStack = put []
    step "+" = processTops (+)
    step "-" = processTops (-)
    step "*" = processTops (*)
    step t = readSafe t >>= push
    processTops op = flip op <$> pop <*> pop >>= push

evalRPNMany :: [Text] -> EnvVars -> Text
evalRPNMany txts env = reportEvalResults $
    evalState (runExceptT (runReaderT (mapM evalOnce txts) env)) []
  where
    evalOnce txt = (fromText txt <> ) <$>
      (buildOK <$> evalRPNOnce txt) `catchError` (pure . buildErr)
    buildOK res = " = " <> showb res
    buildErr err = " Error: " <> showb err

reportEvalResults :: Either EvalError [Builder] -> Text
reportEvalResults (Left e) = "Error: " <> showt e  
reportEvalResults (Right b) = toText $ unlinesB b


e1 :: EnvVars
e1 = [
  ("x", 1),
  ("y", 2)
  ]

r :: IO ()
r = do
  once
  where
    once = do
      ts <- TIO.getLine
      when (ts /= "exit") $ do 
        TIO.putStr $ evalRPNMany [ts] e1
        once
      

t1 :: [Text]
t1 = [
  "1 2 +",
  "x y +",
  "adf",
  "3"
  ]