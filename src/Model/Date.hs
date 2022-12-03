{-# LANGUAGE RecordWildCards #-}
model Model.Date
    (
        -- * Types
        Date,
        -- * Date API
        init,
        resetDate,
        getDate,
        dateDown
    )
    where

import Prelude hiding (init)

data Date = Date
    {
        dKitchen :: Int,    -- total number of kitchen boards
        dScore :: Int       -- score
    }
    deriving (Eq, Ord, Show)

init :: Int -> Date 
init number = Date number 0

getDate :: Date -> Int
getDate Date {..} = dKitchen

dateDown :: Date -> Date 
dateDown d = 
    if dKitchen d - 1 > 0
        then d {dKitchen = dKitchen d - 1}
    else
        d {dKitchen = 0}

-- ? dScore = 0
resetDate :: Int -> Date -> Date
resetDate number d = d {dKitchen = number}