module Control where

import Brick hiding (Result)
import qualified Brick.Types as T
-- import System.Random hiding(next)
import qualified Graphics.Vty as V


import Model
import Model.Kitchen
import Model.Budget
import Model.Date
import Model.Earning
import Model.Player
import Control.Monad.IO.Class (MonadIO(liftIO))

control :: PlayerState -> BrickEvent n Tick -> EventM n (Next PlayerState)
control ps event = case event of
    T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (move up    ps)
    T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (move down  ps)
    T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move left  ps)
    T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move right ps)
    T.VtyEvent (V.EvKey (V.KChar 'a') _) -> nextPlace ps   =<< liftIO (kitchenPlaceA ps)
    T.VtyEvent (V.EvKey (V.KChar 'b') _) -> nextPlace ps   =<< liftIO (kitchenPlaceB ps)
    T.VtyEvent (V.EvKey (V.KChar 'p') _) -> nextPrepare ps   =<< liftIO (kitchenPrepare ps)
    T.VtyEvent (V.EvKey (V.KChar 'k') _) -> nextCook ps   =<< liftIO (kitchenCook ps)
    T.VtyEvent (V.EvKey (V.KChar 's') _) -> nextSeason ps   =<< liftIO (kitchenSeason ps)
    T.VtyEvent (V.EvKey (V.KChar 'c') _) -> nextClean ps   =<< liftIO (kitchenClean ps)
    T.VtyEvent (V.EvKey (V.KChar 'e') _) -> nextServe ps   =<< liftIO (kitchenServe ps)
    T.VtyEvent (V.EvKey (V.KChar ' ') _) -> nextRest ps   =<< liftIO (kitchenRest ps)
    T.VtyEvent (V.EvKey (V.KChar 'v') _) -> nextConfirm ps   =<< liftIO (kitchenConfirm ps)
    T.VtyEvent (V.EvKey V.KEnter _) -> nextRestart ps  =<< liftIO (kitchenRestart ps)

    T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt ps
    _                               -> Brick.continue ps

move :: (Pos -> Pos) -> PlayerState -> PlayerState
move f ps = ps { psPos = f (psPos ps) }

getStrat :: PlayerState -> Strategy
getStrat ps = pStrat (psChef ps)

getPos :: PlayerState -> IO Pos
getPos ps  = getStrat ps (psPos ps) (psKitchen ps) 

kitchenPlaceA :: PlayerState -> IO(Result Kitchen)
kitchenPlaceA ps = 
    if (pBudget (psChef ps)) == 0 then return Retry
    else putIngredientA (psKitchen ps) (psBudget ps) <$> getPos ps

kitchenPlaceB :: PlayerState -> IO(Result Kitchen)
kitchenPlaceB ps = 
    if (pBudget (psChef ps)) == 0 then return Retry
    else putIngredientB (psKitchen ps) (psBudget ps) <$> getPos ps

kitchenPrepare :: PlayerState -> IO(Result Kitchen)
kitchenPrepare ps = 
    if (pBudget (psChef ps)) == 0 then return Retry
    else putPrepare (psKitchen ps) (psBudget ps) <$> getPos ps

kitchenCook :: PlayerState -> IO(Result Kitchen)
kitchenCook ps = 
    if (pBudget (psChef ps)) == 0 then return Retry
    else putCook (psKitchen ps) (psBudget ps) <$> getPos ps

kitchenSeason :: PlayerState -> IO(Result Kitchen)
kitchenSeason ps = 
    if (pBudget (psChef ps)) == 0 then return Retry
    else putSeason (psKitchen ps) (psBudget ps) <$> getPos ps

kitchenClean :: PlayerState -> IO(Result Kitchen)
kitchenClean ps = 
    if (pBudget (psChef ps)) == 0 then return Retry
    else putClean (psKitchen ps) (psBudget ps) <$> getPos ps

kitchenServe :: PlayerState -> IO(Result Kitchen)
kitchenServe ps = 
    if (pBudget (psChef ps)) == 0 then return Retry
    else putServe (psKitchen ps) (psBudget ps) <$> getPos ps

kitchenRestart ps =  putRestart (psKitchen ps) <$> getPos ps

kitchenConfirm ps =  putConfirm (psKitchen ps) <$> getPos ps

kitchenRest ps = putRest (psKitchen ps) <$> getPos ps
-------------------------------------------------------------------------------
placeUpdatePS :: PlayerState -> PlayerState
placeUpdatePS ps = ps {psBudget = budgetDown (psBudget ps) 20}

prepareUpdatePS :: PlayerState -> PlayerState
prepareUpdatePS ps = ps {psBudget = budgetDown (psBudget ps) 5}

cookUpdatePS :: PlayerState -> PlayerState
cookUpdatePS ps = ps {psBudget = budgetDown (psBudget ps) 10}

seasonUpdatePS :: PlayerState -> PlayerState
seasonUpdatePS ps = ps {psBudget = budgetDown (psBudget ps) 5}

serveUpdatePS :: PlayerState -> PlayerState
serveUpdatePS ps = ps {psBudget = budgetDown (psBudget ps) 5,
                        psEarning = add (psEarning ps)}

cleanUpdatePS :: PlayerState -> PlayerState
cleanUpdatePS ps = ps {psBudget = budgetDown (psBudget ps) 10}

restUpdatePS :: PlayerState -> PlayerState
restUpdatePS ps = ps {psBudget = budgetUp (psBudget ps) 100,
              psDate = dateDown (psDate ps),
              psBudgetLow = False}

restartUpdatePS :: PlayerState -> PlayerState
restartUpdatePS ps = ps {psBudget = budgetUp (psBudget ps) 100,
                psDate = resetDate 3 (psDate ps),
                psEarning = resetEarning (psEarning ps),
                psOver = False }

needBudgetPS :: PlayerState -> Int -> PlayerState
needBudgetPS ps b = if (getBudget (psBudget ps)) - b < 0 
    then ps {psBudgetLow = True} 
    else ps {psBudgetLow = False}

confirmUpdatePS :: PlayerState -> PlayerState
confirmUpdatePS ps = ps {psBudgetLow = False}

overUpdatePS :: PlayerState -> PlayerState
overUpdatePS ps = ps {psOver = True,
            psEarning = setMaxEarning (psEarning ps)}            

-------------------------------------------------------------------------------
nextPlace :: PlayerState -> Result Kitchen -> EventM n (Next PlayerState)
nextPlace ps kitchen = case nextMove ps kitchen of
    Right ps' -> if (result (psKitchen ps) == kitchen) then continue (needBudgetPS ps' 20) else continue (placeUpdatePS ps')
    Left res -> halt (ps { psResult = res }) 

nextPrepare :: PlayerState -> Result Kitchen -> EventM n (Next PlayerState)
nextPrepare ps kitchen = case nextMove ps kitchen of
    Right ps' -> if (result (psKitchen ps) == kitchen) then continue (needBudgetPS ps' 5) else continue (prepareUpdatePS ps')
    Left res -> halt (ps { psResult = res }) 

nextCook :: PlayerState -> Result Kitchen -> EventM n (Next PlayerState)
nextCook ps kitchen = case nextMove ps kitchen of
    Right ps' -> if (result (psKitchen ps) == kitchen) then continue (needBudgetPS ps' 10) else continue (cookUpdatePS ps')
    Left res -> halt (ps { psResult = res }) 

nextSeason :: PlayerState -> Result Kitchen -> EventM n (Next PlayerState)
nextSeason ps kitchen = case nextMove ps kitchen of
    Right ps' -> if (result (psKitchen ps) == kitchen) then continue (needBudgetPS ps' 5) else continue (seasonUpdatePS ps')
    Left res -> halt (ps { psResult = res }) 

nextServe :: PlayerState -> Result Kitchen -> EventM n (Next PlayerState)
nextServe ps kitchen = case nextMove ps kitchen of
    Right ps' -> if (result (psKitchen ps) == kitchen) then continue (needBudgetPS ps' 5) else continue (serveUpdatePS ps')
    Left res -> halt (ps { psResult = res }) 

nextClean :: PlayerState -> Result Kitchen -> EventM n (Next PlayerState)
nextClean ps kitchen = case nextMove ps kitchen of
    Right ps' -> if (result (psKitchen ps) == kitchen) then continue (needBudgetPS ps' 10) else continue (cleanUpdatePS ps')
    Left res -> halt (ps { psResult = res }) 

nextRest :: PlayerState -> Result Kitchen -> EventM n (Next PlayerState)
nextRest ps kitchen = case nextMove ps kitchen of
    Right ps' -> if ( getDate (psDate ps) == 0) then continue (overUpdatePS ps') else continue (restUpdatePS ps')
    Left res -> halt (ps { psResult = res }) 

nextConfirm :: PlayerState -> Result Kitchen -> EventM n (Next PlayerState)
nextConfirm ps kitchen = case nextMove ps kitchen of
    Right ps' -> continue (confirmUpdatePS ps')
    Left res -> halt (ps { psResult = res }) 

nextRestart :: PlayerState -> Result Kitchen -> EventM n (Next PlayerState)
nextRestart ps kitchen = case nextMove ps kitchen of
    Right ps' -> continue (restartUpdatePS ps')
    Left res -> halt (ps { psResult = res }) 




