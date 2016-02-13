module Main where

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Colors
import HGE2D.ShapeFactory
import HGE2D.Engine

{- Very basic example showing a "Hello World" -}
--------------------------------------------------------------------------------

--in here we are going to store all data of the game
data GameState = GameState
    { gsSize :: (Double, Double) -- current size of the entire game in pixels
    , time   :: Millisecond      -- current time of the game
    }

--define our initial state
gs1 = GameState { time = 0, gsSize = (0, 0) }

--define all functions of the engine for usage of our state
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
      myGetTitle _ = "Welcome to Example1" --title of the games window
      myGetW = fst . gsSize -- how to retrieve the games window width
      myGetH = snd . gsSize -- hot to retrieve the games window height
      myGetTime = time -- how to retrieve the games time
      mySetTime ms gs = gs { time = ms } -- how to set the games time
      myMoveTime _ = id -- our game won't react to time changes
      myClick _ _ = id -- nor clicks
      myHover _ _ = id -- nor hovering
      myDrag _ _ = id -- nor draging
      myResize w h gs = gs { gsSize = (realToFrac w, realToFrac h) } -- how to resize our game
      myGetSize = gsSize -- and get its size
      myToGlInstr _ = RenderMany -- render our game by using multiple instructions
          [ RenderColorize colorWhite -- rendering a white
          , rectangle 0.3 0.3 -- rectangle
          , RenderColorize colorRed -- and a red
          , RenderText "Hello World" -- "Hello World"
          ]
--------------------------------------------------------------------------------
main = runEngine es1 gs1
