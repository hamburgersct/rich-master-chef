{-# LANGUAGE RecordWildCards #-}
model Model.Budget
    (
        -- * Types
        Budget,
        -- * Budget APIs
        init,
        getBudget,
        bugetUp,
        bugetDown
    )
    where
import Prelude hiding (init)

data Budget = Budget
    {
        bKitchen    :: Int,     -- total number of kitchen boards
        bMax        :: Int,     --
        bMin        :: Int
    }
    deriving (Eq, Ord, Show)

init :: Int -> Budget
init num = Budget num 100 0

getBudget :: Budget -> Int
getBudget Budget {..} = bKitchen

bugetUp :: Budget -> Int -> Budget
bugetUp b bUp = 
    if bKitchen b + bUp > bMax b 
        then b {bKitchen = bMax b}
        else b {bKitchen = bKitchen b + bUp}

bugetDown :: Budget -> Int -> Budget
bugetDown b bDown =
    if bKitchen b - bDown < bMin b 
        then b {bKitchen = bMin b}
        else b {bKitchen = bKitchen b - bDown}