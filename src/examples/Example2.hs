module Main where

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Colors
import HGE2D.Classes
import HGE2D.Instances ()
import HGE2D.Shapes
import HGE2D.Render
import HGE2D.Engine

{- Example showing mouse interactions with the game state -}
--------------------------------------------------------------------------------

--in here we are going to store all data of the game
data GameState = GameState
    { gsSize    :: (Double, Double) -- current size of the entire game in pixels
    , time      :: Millisecond      -- current time of the game
    , isClicked :: Bool             -- whether the rectangle has been clicked
    , pos       :: RealPosition     -- the position of the rectangle
    }

--define our initial state
gs2 = GameState
   { time = 0
   , gsSize = (0, 0)
   , pos = (0, 0)
   , isClicked = False
   }

--define all functions of the engine for usage of our state
es2 = EngineState
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
      myGetTitle _ = "Welcome to Example2" --title of the games window
      myGetTime = time -- how to retrieve the games time
      mySetTime ms gs = gs { time = ms } -- how to set the games time
      myMoveTime _ = id -- our game won't react to time changes
      myClick _ _ gs = gs { isClicked = not $ isClicked gs } -- toggle the isClicked Bool on click
      myMouseUp _ _ gs = gs { isClicked = not $ isClicked gs } --also toggle it on release again
      myHover x y gs = gs { pos = (x, y) } -- store the hover position
      myDrag _ _ gs = gs -- don't react to draging
      myKeyDown _ _ _ gs = gs { isClicked = not $ isClicked gs } -- also toggle clicked with a key press
      myKeyUp _ _ _ gs = gs { isClicked = not $ isClicked gs } --also toggle clicked with key release
      myResize (w, h) gs = gs { gsSize = (realToFrac w, realToFrac h) } -- how to resize our game
      myGetSize = gsSize -- and get its size
      myToGlInstr gs = withCamera es2 gs $ RenderPreserve $ RenderMany -- render with a camera and while preserving changes
          [ RenderColorize color -- a colored
          , RenderTranslate (realToFrac $ getX $ pos gs) (realToFrac $ getY $ pos gs) -- moved to pos
          , rectangle 30 30 -- rectangle of 30px with and height
          ]
        where -- the color of the rectangle depends on the click state
          color | isClicked gs = colorWhite
                | otherwise    = colorGreen

--------------------------------------------------------------------------------
main = runEngine es2 gs2
