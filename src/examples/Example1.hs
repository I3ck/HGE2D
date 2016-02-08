module Main where

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Colors
import HGE2D.Classes
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

--------------------------------------------------------------------------------

--this instance shall always return the window title of your game
instance HasTitle GameState where
    getTitle _ = "Welcome to Example1"

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
    click _ _ gs = gs
    hover _ _ gs = gs
    drag _ _ gs = gs

--enable the engine to pass window resizes to the game
instance Resizeable GameState where
    resize w h gs = gs { gsSize = (realToFrac w, realToFrac h) }
    getSize gs = gsSize gs

--how the state shall be rendered, in our example a simple "Hello World"
instance GlInstructable GameState where
    toGlInstruction _ = RenderMany
       [ RenderColorize colorWhite
       , rectangle 0.3 0.3
       , RenderColorize colorRed
       , RenderText "Hello World"
       ]

--------------------------------------------------------------------------------

main = do
    let initialState = GameState { time = 0, gsSize = (0, 0) }
    runEngine initialState
