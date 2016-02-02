{-# LANGUAGE ConstraintKinds #-}
module HGE2D.Classes where

import HGE2D.Types
import HGE2D.Datas

--------------------------------------------------------------------------------

class MouseInteract a where
    hover, click, drag :: PosX -> PosY -> a -> a

--------------------------------------------------------------------------------

class Resizeable a where ---TODO rename
    resize :: Width -> Height -> a -> a ---TODO (Width, Height)
    getSize :: a -> (Width, Height)

--------------------------------------------------------------------------------

class Dynamic a where
    moveInTime :: Millisecond -> a -> a

--------------------------------------------------------------------------------

class HasTime a where
    getTime    :: a -> Millisecond
    setTime    :: Millisecond -> a -> a

--------------------------------------------------------------------------------

class HasSize a where
    getW :: a -> Width
    getH :: a -> Height

--------------------------------------------------------------------------------

class HasTitle a where
    getTitle :: a -> String

--------------------------------------------------------------------------------

class GlRender a where
    glRender :: a -> IO ()

--------------------------------------------------------------------------------

class GlInstructable a where
    toGlInstruction :: a -> RenderInstruction

--------------------------------------------------------------------------------


type EngineState a = 
    ( MouseInteract a
    , GlInstructable a
    , Resizeable a
    , Dynamic a
    , HasTime a
    , HasSize a
    , HasTitle a
    )

