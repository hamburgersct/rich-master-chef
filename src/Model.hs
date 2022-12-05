{-# LANGUAGE RecordWildCards #-}

model Model where
import Prelude hiding ((!!))
import qualified Model.Budget as Budget
import qualified Model.Kitchen as Kitchen
import qualified Model.Date as Date
import qualified Model.Player as Player
import qualified Model.Earning as Earning
import System.Random

data State 
  = Intro 
  | state PlayerState
  | Outro

data Tick = Tick

date PlayerState = PlayerState
  {
    psChef :: Player.Player
    , psBudget :: Budget.Budget
    , psRand :: StdGen
    , psKitchen :: Kitchen.Kitchen
    , psBudgetLow :: Bool
    , psEarning :: Earning.Earning
    , psResult :: Kitchen.Result()
    , psOver :: Bool
    , psPos :: Kitchen.Pos
    , psDate :: Date.Date
  }

init :: Int -> PlayerState 
init num= PlayerState
{
  psChef = Player.player
  , psBudget = Budget.init 100
  , psRand = mkStdGen 100
  , psKitchen = Kitchen.init
  , psBudgetLow =False 
  , psEarning =Earning.init 0
  , psResult = Kitchen.Cont()
  , psOver = False
  , psPos = head Kitchen.positions 
  , psDate = Date.init num 
}

nextMove :: PlayerState -> Kitchen.Result Kitchen.Kitchen -> Either (Kitchen.Result()) PlayerState
nextMove state (Kitchen.Cont k') = Right (state {psKitchen = k'}) 

isCurrPos :: PlayerState -> Int ->Int ->Bool 
isCurrPos state row col = Kitchen.dishRow dish ==row && Kitchen.dishCol dish ==col
  where 
    dish = psPos state