module Main where

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Colors
import HGE2D.Classes
import HGE2D.Instances ()
import HGE2D.ShapeFactory
import HGE2D.Render
import HGE2D.Engine

{- Example showing dynamic changes by moving the rectanlge
   First we are going to define our GameState and instances,
   and finally run the engine
-}
--------------------------------------------------------------------------------

--in here we are going to store all data of the game
data GameState = GameState
    { gsSize    :: (Double, Double) -- current size of the entire game in pixels
    , time      :: Millisecond      -- current time of the game
    , isClicked :: Bool             -- whether the rectangle has been clicked
    , moveUp    :: Bool             -- whether the rectangle is moving up
    , pos       :: RealPosition     -- the position of the rectangle
    }

gs3 = GameState
   { time = 0
   , gsSize = (0, 0)
   , pos = RealPosition 0 0
   , isClicked = False
   , moveUp = False
   }

es3 = EngineState
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
      myGetTitle _ = "Welcome to Example3"
      myGetW = fst . gsSize
      myGetH = snd . gsSize
      myGetTime = time
      mySetTime ms gs = gs { time = ms }
      myMoveTime ms gs = gs { pos = newPos, moveUp = newMoveUp }
        where
          newMoveUp | realY oldPos < 1                 && moveUp gs         = False
                    | realY oldPos > (snd $ gsSize gs) && (not (moveUp gs)) = True
                    | otherwise = moveUp gs

          newPos | moveUp gs  = RealPosition (realX oldPos) (realY oldPos - realToFrac ms / 30)
                 | otherwise  = RealPosition (realX oldPos) (realY oldPos + realToFrac ms / 10)

          oldPos = pos gs
      myClick _ _ gs = gs { isClicked = not $ isClicked gs }
      myHover x y gs = gs { pos = RealPosition x y }
      myDrag _ _ gs = gs
      myResize w h gs = gs { gsSize = (realToFrac w, realToFrac h) }
      myGetSize = gsSize
      myToGlInstr gs = withCamera es3 gs $ RenderPreserve $ RenderMany
          [ RenderColorize color
          , RenderTranslate (realToFrac $ getX $ pos gs) (realToFrac $ getY $ pos gs)
          , rectangle 30 30
          ]
        where
          color | isClicked gs = colorWhite
                | otherwise    = colorGreen

--------------------------------------------------------------------------------
main = runEngine es3 gs3
