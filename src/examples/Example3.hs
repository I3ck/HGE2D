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

--------------------------------------------------------------------------------

--this instance shall always return the window title of your game
instance HasTitle GameState where
    getTitle _ = "Welcome to Example2"

--should return the current size of the game window
instance HasSize GameState where
    getW gs = fst $ gsSize gs
    getH gs = snd $ gsSize gs

--used to know and initalize the game's time
instance HasTime GameState where
    getTime gs = time gs
    setTime ms gs = gs { time = ms }

---moving the rectangle up/down in a pendulum like motion
instance Dynamic GameState where
    moveInTime ms gs = gs { pos = newPos, moveUp = newMoveUp }
      where
        newMoveUp | realY oldPos < 1                 && moveUp gs         = False
                  | realY oldPos > (snd $ gsSize gs) && (not (moveUp gs)) = True
                  | otherwise = moveUp gs

        newPos | moveUp gs  = RealPosition (realX oldPos) (realY oldPos - realToFrac ms / 30)
               | otherwise  = RealPosition (realX oldPos) (realY oldPos + realToFrac ms / 10)

        oldPos = pos gs

--react to mouse input
instance MouseInteract GameState where
    click _ _ gs = gs { isClicked = not $ isClicked gs }
    hover x y gs = gs { pos = RealPosition x y }
    drag _ _ gs = gs

--enable the engine to pass window resizes to the game
instance Resizeable GameState where
    resize w h gs = gs { gsSize = (realToFrac w, realToFrac h) }
    getSize gs = gsSize gs

--how the state shall be rendered, in our example a simple "Hello World"
instance GlInstructable GameState where
    toGlInstruction gs = withCamera gs $ RenderPreserve $ RenderMany
        [ RenderColorize color
        , RenderTranslate (realToFrac $ getX $ pos gs) (realToFrac $ getY $ pos gs)
        , rectangle 30 30
        ]
      where
        color | isClicked gs = colorWhite
              | otherwise    = colorGreen

--------------------------------------------------------------------------------

main = do
    let initialState = GameState 
                       { time = 0
                       , gsSize = (0, 0)
                       , pos = RealPosition 0 0
                       , isClicked = False
                       , moveUp = False
                       }
    runEngine initialState
