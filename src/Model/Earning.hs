{-# LANGUAGE RecordWildCards #-}
module Model.Earning
    (
        -- * Types
        Earning,
        -- * Earning APIs
        init,
        add,
        detract,
        resetEarning,
        setMaxEarning,
        getCurrEarning,
        getMaxEarning
    )
    where

import Prelude hiding (init)

------------------------------
-- Earning model
------------------------------

data Earning = Earning
    {
        eKitchen    :: Int,  -- current earning
        eMax        :: Int  -- max earning on record
    }
    deriving (Eq, Ord, Show)

init :: Int -> Earning
init num = Earning num num

add :: Earning -> Earning
add e = e {eKitchen = eKitchen e + 100}

detract :: Earning -> Earning
detract e = e {eKitchen = eKitchen e - 1}

resetEarning :: Earning -> Earning
resetEarning e = e {eKitchen = 0}

setMaxEarning :: Earning -> Earning
setMaxEarning e = e { eMax = if (curr > max) then curr else max}
  where curr = getCurrEarning e
        max = getMaxEarning e

getCurrEarning :: Earning -> Int
getCurrEarning Earning {..} = eKitchen

getMaxEarning :: Earning -> Int
getMaxEarning Earning {..} = eMax