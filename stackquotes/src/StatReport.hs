{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module StatReport where

import QuoteData (QField, QuoteData, day, QField(..), field2fun)
import Data.Ord (comparing)
import Data.Foldable (minimumBy, maximumBy)
import Data.Time (diffDays)
import Fmt

decimalPlacesFloating = 2
data StatValue = StatValue {
    decimalPlace :: Int,
    value :: Double
}


data StatEntry = StatEntry {
    qfield :: QField,
    meanVal :: StatValue,
    minVal :: StatValue,
    maxVal :: StatValue,
    daysBetweenMinMax :: Int
}

mean :: (Fractional a, Foldable t) => t a -> a
mean xs = sum xs / fromIntegral (length xs)


computeMinMaxDays :: (Ord a, Foldable t) => (QuoteData -> a) -> t QuoteData -> (a, a, Int)
computeMinMaxDays get quotes = (get minQ, get maxQ, days)
    where
        cmp = comparing get
        minQ = minimumBy cmp quotes
        maxQ = maximumBy cmp quotes
        days = fromInteger $ abs $ diffDays (day minQ) (day maxQ)
        
statInfo :: (Functor t, Foldable t) => t QuoteData -> [StatEntry]
statInfo quotes = fmap qFieldStatInfo [minBound .. maxBound] 
     where
         decimalPlacesByQField filed = if filed == Volume then 0 else decimalPlacesFloating
         qFieldStatInfo qfield = 
            let 
                get = field2fun qfield
                decimalPlace = decimalPlacesByQField qfield
                (mn, mx, daysBetweenMinMax) = computeMinMaxDays get quotes
                decPlace = decimalPlacesByQField qfield
                meanVal = StatValue decimalPlacesFloating
                                    (mean $ fmap get quotes)
                minVal = StatValue decimalPlace mn
                maxVal = StatValue decimalPlace mx
                in StatEntry {..}

instance Buildable StatValue where
    build sv = fixedF (decimalPlace sv) (value sv)

instance Buildable StatEntry where
    build StatEntry {..} =
        "Stats for "+||qfield||+": "
            +|meanVal|+" (mean), "
            +|minVal|+" (min), "
            +|maxVal|+" (max), "
            +|daysBetweenMinMax|+" (days)" 
            