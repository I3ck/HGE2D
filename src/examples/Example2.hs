module Main where

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Colors
import HGE2D.Classes
import HGE2D.Instances
import HGE2D.ShapeFactory
import HGE2D.Engine

{- Example showing mouse interactions with the game state
   First we are going to define our GameState and instances,
   and finally run the engine
-}
--------------------------------------------------------------------------------

--in here we are going to store all data of the game
data GameState = GameState
    { gsSize    :: (Double, Double) -- current size of the entire game in pixels
    , time      :: Millisecond      -- current time of the game
    , isClicked :: Bool             -- whether the rectangle has been clicked
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

--our first example is static, therefore we just gonna leave the game as-is
instance Dynamic GameState where
    moveInTime _ gs = gs

--also we won't react to any input
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
    toGlInstruction gs = RenderPreserve
       [ RenderColorize color
       , RenderTranslate (transformX gs (getX $ pos gs)) 
                         (transformY gs (getY $pos gs))
       , rectangle 0.3 0.3
       ]
      where
        color | isClicked gs = colorWhite
              | otherwise    = colorGreen

--------------------------------------------------------------------------------

--transform world coords to gl coords
transformX :: GameState -> Double -> GlPosX
transformX gs x = realToFrac $   2.0 * x / (fst $ gsSize gs) - 1.0

transformY :: GameState -> Double -> GlPosY
transformY gs y = realToFrac $ - 2.0 * y / (snd $ gsSize gs) + 1.0

--------------------------------------------------------------------------------

main = do
    let initialState = GameState 
                       { time = 0
                       , gsSize = (0, 0)
                       , pos = RealPosition 0 0
                       , isClicked = False 
                       }
    runEngine initialState
