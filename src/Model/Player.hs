module Model.Player where

import Model.Kitchen

data Player = Player
    {
        pBudget     :: Int,
        pStrat      :: Strategy
    }

type Strategy = Pos         -- current player cursor
                -> Kitchen  -- current kitchen board
                -> IO Pos   -- next move

player :: Player
player = Player 100 (\p _ -> return p)