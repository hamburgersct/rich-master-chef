{-# LANGUAGE DeriveFunctor #-}
module Model.Kitchen
    ( -- * Types
        Kitchen,
        Dish (..),
        Pos (..),
        Result (..),
      -- * Kitchen API
        dim,
        (!),
        init,
        putGradientA,
        putGradientB,
        -- putGradientC,
        putPrepare,
        putCook,
        putSeason,
        putServe,
        -- * putDebug
        putRest,
        putClean,
        putConfirm,
        positions,
        -- * putEat
        putRestart,
        emptyPositions,
        takenPositions,
        result,
        -- * Pointer moves
        up,
        down,
        left,
        right
    )
    where

import Prelude hiding (init)
import Model.Budget hiding (init)
import Model.Earning hiding (init)
import qualified Data.Map as M 

------------------------------
--  Kitchen Board
------------------------------

type Kitchen = M.Map Pos Dish

-- * Dishes
data Dish
    = GradientA0        -- unprepared gradient for dish A (soup)
    | GradientA1        -- prepared gradient for dish A (soup)
    | Prepared_raw_us_A -- prepared but still not-fully-cooked & unseasoned dish A
    | Prepared_cooked_us_A  -- prepared & cooked but unseasoned dish A
    | Prepared_raw_s_A -- prepared, seasoned but not-fully-cooked dish A
    | DishA             -- Soup (good to serve)
    -- | ServedDishA       -- served Soup
    | GradientB         -- unprepared gradient for dish B (Sashimi)
    | Prepared_us_B     -- prepared but unseasoned dish B
    | DishB             -- Sashimi (ready to serve)
    -- | ServedDishB       -- served sashimi
    -- | GradientC0        -- unprepared gradient for dish C (steak)
    -- | GradientC1        -- prepared gradient for dish C
    -- | Prepared_raw_us_C -- prepared, not-fully-cooked, unseasoned dish C
    -- | Prepared_
    | Bad               -- Mistreated dishes
    deriving (Eq, Show)

-- * Positions
data Pos = Pos
    {
        dishRow :: Int,       -- 1 <= dishRow <= dim
        dishCol :: Int        -- 1 <= dishCol <= dim
    }
    deriving (Eq, Ord)

(!) :: Kitchen -> Pos -> Maybe Dish
kitchen ! pos = M.lookup pos kitchen

dim :: Int
dim = 3

positions :: [Pos]
positions = [Pos row col | row <- [1 .. dim], col <- [1 .. dim]]

emptyPositions :: Kitchen -> [Pos]
emptyPositions kitchen = [pos | pos <- positions, M.notMember pos kitchen]

takenPositions :: Kitchen -> [Pos]
takenPositions kitchen = [pos | pos <- positions, M.member pos kitchen]

init :: Kitchen
init = M.empty

------------------------------
--  Playing Logistics
------------------------------

data Result a 
    = Retry
    | Cont a 
    deriving (Eq, Functor, Show)

updateDishPrep :: Dish -> Maybe Dish
updateDishPrep d =
    case d of
        GradientA0      -> Just GradientA1
        GradientB       -> Just Prepared_us_B
        _               -> Just Bad

updateDishCook :: Dish -> Maybe Dish
updateDishCook d =
    case d of
        GradientA1      -> Just Prepared_raw_us_A
        Prepared_raw_us_A   -> Just Prepared_cooked_us_A
        Prepared_raw_s_A    -> Just DishA
        _                   -> Just Bad

updateDishSeason :: Dish -> Maybe Dish
updateDishSeason d =
    case d of
        Prepared_raw_us_A   -> Just Prepared_raw_s_A
        Prepared_cooked_us_A -> Just DishA
        Prepared_us_B       -> Just DishB
        _                   -> Just Bad

-- Operations
putGradientA :: Kitchen -> Budget -> Pos -> Result Kitchen
putGradientA kitchen buget pos = 
    if (getBudget buget) >= 20
        then
            case M.lookup pos kitchen of
                Nothing         -> result (M.insert pos GradientA0 kitchen)
                Just _          -> result kitchen
        else
            result kitchen

putGradientB :: Kitchen -> Budget -> Pos -> Result Kitchen
putGradientB kitchen buget pos = 
    if (getBudget buget) >= 20
        then
            case M.lookup pos kitchen of
                Nothing         -> result (M.insert pos GradientB kitchen)
                Just _          -> result kitchen
        else
            result kitchen

putPrepare :: Kitchen -> Budget -> Pos -> Result Kitchen
putPrepare kitchen buget pos = 
    if (getBudget buget) >= 5
        then
            case M.lookup pos kitchen of
                Just _          -> result (M.update updateDishPrep pos kitchen)
                Nothing         -> result kitchen
        else
            result kitchen

putSeason :: Kitchen -> Budget -> Pos -> Result Kitchen
putSeason kitchen buget pos = 
    if (getBudget buget) >= 5
        then
            case M.lookup pos kitchen of
                Just _          -> result (M.update updateDishSeason pos kitchen)
                Nothing         -> result kitchen
        else
            result kitchen

putCook :: Kitchen -> Budget -> Pos -> Result Kitchen
putCook kitchen buget pos =
    if (getBudget buget) >= 10
        then
            case M.lookup pos kitchen of
                Just _          -> result (M.update updateDishCook pos kitchen)
                Nothing         -> result kitchen
        else
            result kitchen

putServe :: Kitchen -> Budget -> Pos -> Result Kitchen
putServe kitchen buget pos = 
    if (getBudget buget) >= 5
        then
            case M.lookup pos kitchen of
                Just DishA      -> result (M.delete pos kitchen)
                Just DishB      -> result (M.delete pos kitchen)
                _               -> result kitchen
        else
            result kitchen

putClean :: Kitchen -> Budget -> Pos -> Result Kitchen
putClean kitchen buget pos =
    if (getBudget buget) >= 10
        then
            case M.lookup pos kitchen of
                Just Bad        -> result (M.delete pos kitchen)
                _               -> result kitchen
        else
            result kitchen

-- * Rest 
-- * Rest and clean remaining dishes in kitchen

putRest :: Kitchen  -> Pos -> Result Kitchen
putRest kitchen _ = result kitchen

putConfirm :: Kitchen -> Pos -> Result Kitchen
putConfirm kitchen _ = result kitchen

putRestart :: Kitchen -> Pos -> Result Kitchen
putRestart _ _ = result M.empty

result :: Kitchen -> Result Kitchen
result b
    | otherwise = Cont b 

------------------------------
--  Player Moves
------------------------------
up :: Pos -> Pos
up pos = pos 
    {
        dishRow = max 1 (dishRow pos - 1)
    }

down :: Pos -> Pos
down pos = pos 
    {
        dishRow = min dim (dishRow pos + 1)
    }

left :: Pos -> Pos
left pos = pos 
    {
        dishCol = max 1 (dishCol pos - 1)
    }

right :: Pos -> Pos
right pos = pos 
    {
        dishCol = min dim (dishCol pos + 1)
    }
