module View (view, myMap) where

import Brick
import Brick.Widgets.Center 
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode, unicodeBold)

import Text.Printf (printf)

import Model
import Model.Budget
import Model.Earning
import Model.Kitchen
import Model.Date
import Model.Player
import Graphics.Vty hiding (dim)

-- -------------------------------------------------------------------------------
-- view :: PlayerState -> [Widget String]
-- -------------------------------------------------------------------------------
-- view s = [showStats s, field s]

instAttr = attrName "instruction"
scoreAttr = attrName "score"
fieldAttr = attrName "field"
alertAttr = attrName "alert"

view :: PlayerState -> [Widget String]
view s = [gameOver s, withAttr alertAttr $ (canNotHold s), center $ padRight (Pad 2) inst_sc <+> (withAttr fieldAttr(field s))]
  where inst_sc = hLimit 50 $ vTile[withAttr instAttr $ showInst, withAttr scoreAttr $ (showStats s)]

myMap :: PlayerState -> AttrMap
myMap _ = attrMap defAttr
  [
    (instAttr, fg yellow ), 
    (scoreAttr, fg green ),
    (fieldAttr, fg brightBlue),
    (alertAttr, fg red)
  ]

field :: PlayerState -> Widget String
field s = 
  withBorderStyle unicode $
    borderWithLabel (str ("Master-Chef")) $
      vTile [ mkRow s row | row <- [1..dim] ]

gameOver :: PlayerState -> Widget String
gameOver s = 
  if psOver s == True
    then withBorderStyle unicodeBold
        $ borderWithLabel (str ("Game is Over!"))
        $ center
        $ vBox [str (getEnd s)]
    else emptyWidget

canNotHold :: PlayerState -> Widget String
canNotHold s = 
  if psBudgetLow s == True
    then(withBorderStyle unicodeBold
        $ borderWithLabel (str ("Alert!"))
        $ center
        $ vBox [str (getSleep s)]) 
    else emptyWidget

showInst :: Widget String
showInst = withBorderStyle unicodeBold
  $ borderWithLabel (str ("Instructions"))
  $ center
  $ hLimit 80
  $ vBox [str getInst,
          str getInst2,
          str getInst3,
          str getInst4,
          str getInst5,
          str getInst6]

showStats :: PlayerState -> Widget String
showStats s = 
  withBorderStyle unicodeBold
  $ borderWithLabel (str ("Earning"))
  $ center
  $ hLimit 80
  $ vBox [str (getStats s)]

getStats :: PlayerState -> String
getStats s = printf "Current Budget = %d,\nEarning = %d,\nDate = %d\n" (getBudget p) (getCurrEarning er) (getDate d)
      where p = psBudget s
            er = psEarning s
            d = psDate s

--change wording--
getInst :: String
getInst = printf "Press O to plant an Apple,\nPress P to plant a rice,\nPress W to water a plant,\nPress F to apply fertilizer,\nPress C to clean up dead plant,\nPress E to eat up plant\nPress R to reap plant\nBlank Space to Sleep"

getInst2 :: String
getInst2 = printf "\nWhen plants need water their leaves are shrinking \nlike this /\\/\\\n"

getInst3 :: String
getInst3 = printf "\nWhen plants need fertilizer their stems are twisted \n"

getInst4 :: String
getInst4 = printf "\nWhen plants are bugged their leaves are abnormal\nlike this X X \n"

getInst5 :: String
getInst5 = printf "\nWhen plants are dead their leaves are falling\n"

getInst6 :: String
getInst6 = printf "\nToo much water/fertilizer can kill the plant\n"
--change wording--

getEnd :: PlayerState -> String
getEnd s = printf "Your final earning is %d.\nMaxEarning is %d.\nPress Enter to restart the game.\nOr press Esc to exit." (getCurrEarning er) (getMaxEarning er)
      where er = psEarning s

getSleep :: PlayerState -> String
getSleep s = printf "You don't have enough budget now!\n\nPress blank space to sleep.\nPress E to to one of your grown plant.\nPress A to getback to game.\nNow you have %d scores %d budget" (getCurrEarning er) (getBudget p)
      where er = psEarning s
            p = psBudget s

mkCell :: PlayerState -> Int -> Int -> Widget n
mkCell s r c 
  | isCurrPos s r c = withCursor raw 
  | otherwise    = raw 
  where
    raw = mkCell' s r c

mkRow :: PlayerState -> Int -> Widget n
mkRow ps row = hTile [ mkCell ps row i | i <- [1..dim] ]

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withStyle` reverseVideo)

mkCell' :: PlayerState -> Int -> Int -> Widget n
-- mkCell' _ r c = center (str (printf "(%d, %d)" r c))
mkCell' s r c = center (mkDish xoMb)
  where 
    xoMb      = psKitchen s ! Pos r c

mkDish :: Maybe Dish -> Widget n
mkDish Nothing = blockB
mkDish (Just GradientA0) = blockGradientA0
mkDish (Just GradientA1) = blockGradientA1
mkDish (Just Prepared_raw_us_A) = blockNeedCS
mkDish (Just Prepared_cooked_us_A) = blockNeedS
mkDish (Just Prepared_raw_s_A) = blockNeedC
mkDish (Just DishA) = blockCooked
mkDish (Just GradientB) = blockGradientB
mkDish (Just Prepared_us_B) = blockNeedS
mkDish (Just DishB) = blockCooked
mkDish (Just Bad) = blockBad

blockB, blockGradientA0, blockGradientA1, blockCooked, blockNeedCS, blockNeedS, blockServed, blockBad:: Widget n
blockB = vBox (replicate 5 (str "     "))

blockGradientA0  = vBox [ str "     "
                    , str "     "
                    , str "  _  "
                    , str " / \\ "
                    , str " \\_/ "] 
                   
blockGradientA1  = vBox [ str "     "
                    , str "     "
                    , str "  *  "
                    , str " / \\ "
                    , str " \\_/ "]
               
blockGradientB  = vBox [ str "     "
                   , str "     "
                   , str "  x  "
                   , str " / \\ "
                   , str " \\_/ "]  

blockServed = vBox [ str "     "
                   , str "\\   /"
                   , str " \\ / "
                   , str "  |  "
                   , str "  |  "]

blockNeedCS  = vBox [ str "     "
                   , str "     "
                   , str "/\\ /\\"
                   , str "  |  "
                   , str "  |  "]

blockNeedS  = vBox [ str "     "
                   , str "\\   /"
                   , str " \\ / "
                   , str "  /  "
                   , str "  \\  "]

blockNeedC = vBox [ str "     "
                   , str "     "
                   , str "/\\ /\\"
                   , str "  /  "
                   , str "  \\  "]

blockBad = vBox [ str "     "
                     , str "     "
                     , str " X X "
                     , str "  |  "
                     , str "  |  "]

blockCooked =    vBox [ str "     "
                      , str "     "
                      , str " _|_ "
                      , str "(___)"
                      , str "     "]

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget