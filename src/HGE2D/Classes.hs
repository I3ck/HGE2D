{-# LANGUAGE ConstraintKinds #-}
module HGE2D.Classes where

import HGE2D.Types
import HGE2D.Render

--------------------------------------------------------------------------------

class MouseInteract a where
    hover, click, drag :: PosX -> PosY -> a -> a

--------------------------------------------------------------------------------

class Resizeable a where ---TODO rename
    resize :: Width -> Height -> a -> a ---TODO (Width, Height)
    getSize :: a -> (Width, Height)

--------------------------------------------------------------------------------

class HasTime a where
    getTime    :: a -> Millisecond
    setTime    :: Millisecond -> a -> a
    moveInTime :: Millisecond -> a -> a

--------------------------------------------------------------------------------

class HasSize a where
    getW :: a -> Width
    getH :: a -> Height

--------------------------------------------------------------------------------

class HasTitle a where
    getTitle :: a -> String

--------------------------------------------------------------------------------

type EngineState a = 
    ( MouseInteract a
    , GlInstructable a
    , Resizeable a
    , HasTime a
    , HasSize a
    , HasTitle a
    )

