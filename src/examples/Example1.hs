module Main where

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Classes
import HGE2D.Instances
import HGE2D.ShapeFactory
import HGE2D.Engine

--------------------------------------------------------------------------------

data GameState = GameState
    { gsSize :: (Double, Double) -- current size
    , time   :: Millisecond      -- current time
    }

--------------------------------------------------------------------------------

instance HasTitle GameState where
    getTitle _ = "Welcome to Example1"

instance HasSize GameState where
    getW gs = fst $ gsSize gs
    getH gs = snd $ gsSize gs

instance HasTime GameState where
    getTime gs = time gs
    setTime ms gs = gs { time = ms }

instance Dynamic GameState where
    moveInTime ms gs = gs --- TODO change state here (maybe simple counter for the example?)

instance MouseInteract GameState where
    click x y gs = gs
    hover x y gs = gs
    drag x y gs = gs

instance Resizeable GameState where
    resize w h gs = gs { gsSize = (realToFrac w, realToFrac h) }
    getSize gs = gsSize gs

instance GlInstructable GameState where
    toGlInstruction gs = rectangle 0.3 0.3



--------------------------------------------------------------------------------

main = do
    let initialState = GameState { time = 0, gsSize = (0, 0) }
    runEngine initialState
