module Main where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.Environment (getArgs)
import Control 
import View 
import Model

import Graphics.Vty.Attributes
import qualified Graphics.Vty as V
import Brick
import Brick.BChan (newBChan, writeBChan)
import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)

main :: IO()
main = do 
  dateNums <- fromMaybe byDefaultDateNums <$> getDateNums 
  chan <- newBChan 15
  forkIO $ forever $ do 
    writeBChan chan Tick
    threadDelay 200000
  let vty =V.mkVty V.defaultConfig
  initVty <- vty
  result <- customMain initVty vty (Just chan) myApp (Model.init dateNums)
  print (psResult result, psEarning result) 

myApp :: App PlayerState Tick String 
myApp = App
  { appDraw         = view 
  , appChooseCursor = const . const Nothing
  , appHandleEvent  = control 
  , appStartEvent   = return
  , appAttrMap      = myMap
  }

getDateNums :: IO (Maybe Int)
getDateNums =do 
  input <- getArgs 
  case input of 
    (s:_) ->return (readMaybe s)
    _ -> return Nothing

byDefaultDateNums :: Int 
byDefaultDateNums = 3


