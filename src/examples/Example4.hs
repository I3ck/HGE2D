module Main where

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Colors
import HGE2D.Shapes
import HGE2D.Engine
import HGE2D.Render

{- Example showing more advanced rendering -}
--------------------------------------------------------------------------------

--in here we are going to store all data of the game
data GameState = GameState
    { gsSize :: (Double, Double) -- current size of the entire game in pixels
    , time   :: Millisecond      -- current time of the game
    }

--in here we are going to store all data of the game
gs4 = GameState { time = 0, gsSize = (0, 0) }

--define our initial state
es4 = EngineState
    { getTitle = myGetTitle
    , getTime = myGetTime
    , setTime = mySetTime
    , moveTime = myMoveTime
    , click = myClick
    , mUp = myMouseUp
    , hover = myHover
    , drag = myDrag
    , kDown = myKeyDown
    , kUp = myKeyUp
    , resize = myResize
    , getSize = myGetSize
    , toGlInstr = myToGlInstr
    } :: EngineState GameState
  where
      myGetTitle _ = "Welcome to Example4" --title of the games window
      myGetTime = time -- how to retrieve the games time
      mySetTime ms gs = gs { time = ms } -- how to set the games time
      myMoveTime _ = id -- our game won't react to time changes
      myClick _ _ = id -- nor clicks
      myMouseUp _ _ = id --nor mouse up
      myHover _ _ = id -- nor hovering
      myDrag _ _ = id -- nor draging
      myKeyDown _ _ _ = id -- nor key presses
      myKeyUp _ _ _ = id --nor key releases
      myResize (w, h) gs = gs { gsSize = (realToFrac w, realToFrac h) } -- how to resize our game
      myGetSize = gsSize -- and get its size
      myToGlInstr gs = withCamera es4 gs $ RenderMany -- render with a camera and while preserving changes
          [ circleNextToRectangle -- render our circle next to the rectangle
          , whiteRectangle -- as well as the white rectangle
          , allMoved -- and the moved group
          ]
        where

          -- move them all by 100px in x and y direction
          allMoved :: RenderInstruction
          allMoved = RenderPreserve $ RenderMany [RenderTranslate 100 100, circleNextToRectangle]

          -- group the moved circle and the white rectangle
          circleNextToRectangle :: RenderInstruction
          circleNextToRectangle = RenderMany [movedCircle, whiteRectangle]

          -- the circle moved
          movedCircle :: RenderInstruction
          movedCircle = RenderPreserve $ RenderMany [RenderTranslate 50 0, redCircle]

          -- a white rectangle
          whiteRectangle :: RenderInstruction
          whiteRectangle = RenderMany [RenderColorize colorWhite, rectangle 30 30]

          -- a red circle
          redCircle :: RenderInstruction
          redCircle = RenderMany [RenderColorize colorRed, circle 30]

--------------------------------------------------------------------------------

main = runEngine es4 gs4
