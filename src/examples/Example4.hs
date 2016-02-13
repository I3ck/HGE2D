module Main where

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Colors
import HGE2D.ShapeFactory
import HGE2D.Engine
import HGE2D.Render

{- Example showing more advanced rendering
   First we are going to define our GameState and instances,
   and finally run the engine
-}
--------------------------------------------------------------------------------

--in here we are going to store all data of the game
data GameState = GameState
    { gsSize :: (Double, Double) -- current size of the entire game in pixels
    , time   :: Millisecond      -- current time of the game
    }

gs4 = GameState { time = 0, gsSize = (0, 0) }

es4 = EngineState
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
      myGetTitle _ = "Welcome to Example4"
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
      myToGlInstr gs = withCamera es4 gs $ RenderMany
          [ circleNextToRectangle
          , whiteRectangle
          , allMoved
          ]
        where

          allMoved :: RenderInstruction
          allMoved = RenderPreserve $ RenderMany [RenderTranslate 100 100, circleNextToRectangle]

          circleNextToRectangle :: RenderInstruction
          circleNextToRectangle = RenderMany [movedCircle, whiteRectangle]

          movedCircle :: RenderInstruction
          movedCircle = RenderPreserve $ RenderMany [RenderTranslate 50 0, redCircle]

          whiteRectangle :: RenderInstruction
          whiteRectangle = RenderMany [RenderColorize colorWhite, rectangle 30 30]

          redCircle :: RenderInstruction
          redCircle = RenderMany [RenderColorize colorRed, circle 30]

--------------------------------------------------------------------------------

main = runEngine es4 gs4
