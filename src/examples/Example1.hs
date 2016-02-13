module Main where

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Colors
import HGE2D.ShapeFactory
import HGE2D.Engine

{- Very basic example showing a "Hello World"
   First we are going to define our GameState and instances,
   and finally run the engine
-}
--------------------------------------------------------------------------------

--in here we are going to store all data of the game
data GameState = GameState
    { gsSize :: (Double, Double) -- current size of the entire game in pixels
    , time   :: Millisecond      -- current time of the game
    }

gs1 = GameState { time = 0, gsSize = (0, 0) }

es1 = EngineState
    { getTitle = myGetTitle
    , getW = myGetW
    , getH = myGetH
    , getTime = myGetTime
    , setTime = mySetTime
    , moveTime = myMoveTime
    , click = myClick
    , hover = myHover
    , drag = myDrag
    , resize = myResize
    , getSize = myGetSize
    , toGlInstr = myToGlInstr
    } :: EngineState GameState
  where
      myGetTitle _ = "Welcome to Example1"
      myGetW = fst . gsSize
      myGetH = snd . gsSize
      myGetTime = time
      mySetTime ms gs = gs { time = ms }
      myMoveTime _ = id
      myClick _ _ = id
      myHover _ _ = id
      myDrag _ _ = id
      myResize w h gs = gs { gsSize = (realToFrac w, realToFrac h) }
      myGetSize = gsSize
      myToGlInstr _ = RenderMany
          [ RenderColorize colorWhite
          , rectangle 0.3 0.3
          , RenderColorize colorRed
          , RenderText "Hello World"
          ]

--------------------------------------------------------------------------------
main = runEngine es1 gs1
